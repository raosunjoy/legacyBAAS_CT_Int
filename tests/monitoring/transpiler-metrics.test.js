const { TranspilerMetrics } = require('../../src/monitoring/transpiler-metrics');
const EventEmitter = require('events');
const os = require('os');

describe('TranspilerMetrics', () => {
  let metrics;

  beforeEach(() => {
    metrics = new TranspilerMetrics();
  });

  afterEach(() => {
    if (metrics) {
      metrics.cleanup();
    }
  });

  describe('Constructor and Initialization', () => {
    test('should initialize with default metrics', () => {
      expect(metrics.metrics.transpilations.total).toBe(0);
      expect(metrics.metrics.transpilations.successful).toBe(0);
      expect(metrics.metrics.transpilations.failed).toBe(0);
      expect(metrics.metrics.performance.totalTime).toBe(0);
      expect(metrics.metrics.performance.averageTime).toBe(0);
      expect(metrics.metrics.errors.total).toBe(0);
      expect(metrics.metrics.system.memoryUsage).toBeDefined();
      expect(metrics.metrics.system.cpuUsage).toBeDefined();
    });

    test('should extend EventEmitter', () => {
      expect(metrics).toBeInstanceOf(EventEmitter);
    });

    test('should start system monitoring', () => {
      expect(metrics.systemMonitorInterval).toBeDefined();
    });
  });

  describe('Transpilation Metrics', () => {
    test('should record successful transpilation', () => {
      const startTime = Date.now();
      const transpilationTime = 1500;
      
      metrics.recordTranspilation('success', transpilationTime, {
        sourceFile: 'test.cbl',
        targetLanguage: 'javascript',
        linesOfCode: 100
      });

      expect(metrics.metrics.transpilations.total).toBe(1);
      expect(metrics.metrics.transpilations.successful).toBe(1);
      expect(metrics.metrics.transpilations.failed).toBe(0);
      expect(metrics.metrics.performance.totalTime).toBe(transpilationTime);
      expect(metrics.metrics.performance.averageTime).toBe(transpilationTime);
    });

    test('should record failed transpilation', () => {
      const transpilationTime = 500;
      
      metrics.recordTranspilation('failed', transpilationTime, {
        sourceFile: 'test.cbl',
        targetLanguage: 'javascript',
        error: 'Syntax error'
      });

      expect(metrics.metrics.transpilations.total).toBe(1);
      expect(metrics.metrics.transpilations.successful).toBe(0);
      expect(metrics.metrics.transpilations.failed).toBe(1);
      expect(metrics.metrics.performance.totalTime).toBe(transpilationTime);
      expect(metrics.metrics.performance.averageTime).toBe(transpilationTime);
    });

    test('should calculate correct average time', () => {
      metrics.recordTranspilation('success', 1000);
      metrics.recordTranspilation('success', 2000);
      metrics.recordTranspilation('failed', 1500);

      expect(metrics.metrics.transpilations.total).toBe(3);
      expect(metrics.metrics.performance.totalTime).toBe(4500);
      expect(metrics.metrics.performance.averageTime).toBe(1500);
    });

    test('should emit transpilation event', (done) => {
      const transpilationTime = 1000;
      const details = { sourceFile: 'test.cbl' };

      metrics.on('transpilation', (data) => {
        expect(data.status).toBe('success');
        expect(data.duration).toBe(transpilationTime);
        expect(data.details).toEqual(details);
        done();
      });

      metrics.recordTranspilation('success', transpilationTime, details);
    });
  });

  describe('Error Tracking', () => {
    test('should record error', () => {
      const error = new Error('Test error');
      const context = { sourceFile: 'test.cbl', line: 10 };

      metrics.recordError(error, context);

      expect(metrics.metrics.errors.total).toBe(1);
      expect(metrics.metrics.errors.byType['Error']).toBe(1);
      expect(metrics.metrics.errors.recent).toHaveLength(1);
      expect(metrics.metrics.errors.recent[0].message).toBe('Test error');
      expect(metrics.metrics.errors.recent[0].context).toEqual(context);
    });

    test('should track error types', () => {
      metrics.recordError(new SyntaxError('Syntax error'));
      metrics.recordError(new TypeError('Type error'));
      metrics.recordError(new SyntaxError('Another syntax error'));

      expect(metrics.metrics.errors.total).toBe(3);
      expect(metrics.metrics.errors.byType['SyntaxError']).toBe(2);
      expect(metrics.metrics.errors.byType['TypeError']).toBe(1);
    });

    test('should limit recent errors to 100', () => {
      for (let i = 0; i < 150; i++) {
        metrics.recordError(new Error(`Error ${i}`));
      }

      expect(metrics.metrics.errors.recent).toHaveLength(100);
      expect(metrics.metrics.errors.recent[0].message).toBe('Error 50');
      expect(metrics.metrics.errors.recent[99].message).toBe('Error 149');
    });

    test('should emit error event', (done) => {
      const error = new Error('Test error');
      const context = { sourceFile: 'test.cbl' };

      metrics.on('error', (data) => {
        expect(data.error).toBe(error);
        expect(data.context).toEqual(context);
        done();
      });

      metrics.recordError(error, context);
    });
  });

  describe('Performance Monitoring', () => {
    test('should start performance timer', () => {
      const timerId = metrics.startTimer('test-operation');
      
      expect(typeof timerId).toBe('string');
      expect(metrics.activeTimers.has(timerId)).toBe(true);
    });

    test('should end performance timer', () => {
      const timerId = metrics.startTimer('test-operation');
      
      setTimeout(() => {
        const duration = metrics.endTimer(timerId);
        
        expect(typeof duration).toBe('number');
        expect(duration).toBeGreaterThan(0);
        expect(metrics.activeTimers.has(timerId)).toBe(false);
      }, 10);
    });

    test('should record custom metric', () => {
      metrics.recordCustomMetric('custom.test', 42, { unit: 'count' });
      
      expect(metrics.metrics.custom['custom.test']).toBeDefined();
      expect(metrics.metrics.custom['custom.test'].value).toBe(42);
      expect(metrics.metrics.custom['custom.test'].metadata.unit).toBe('count');
    });

    test('should emit performance event', (done) => {
      const timerId = metrics.startTimer('test-operation');

      metrics.on('performance', (data) => {
        expect(data.operation).toBe('test-operation');
        expect(data.duration).toBeGreaterThan(0);
        done();
      });

      setTimeout(() => {
        metrics.endTimer(timerId);
      }, 10);
    });
  });

  describe('System Monitoring', () => {
    test('should update system metrics', () => {
      const initialMemory = metrics.metrics.system.memoryUsage;
      const initialCpu = metrics.metrics.system.cpuUsage;

      metrics.updateSystemMetrics();

      expect(metrics.metrics.system.memoryUsage).toBeDefined();
      expect(metrics.metrics.system.cpuUsage).toBeDefined();
      expect(metrics.metrics.system.timestamp).toBeGreaterThan(0);
    });

    test('should emit system event', (done) => {
      metrics.on('system', (data) => {
        expect(data.memoryUsage).toBeDefined();
        expect(data.cpuUsage).toBeDefined();
        expect(data.timestamp).toBeGreaterThan(0);
        done();
      });

      metrics.updateSystemMetrics();
    });
  });

  describe('Metrics Export', () => {
    beforeEach(() => {
      metrics.recordTranspilation('success', 1000, { sourceFile: 'test1.cbl' });
      metrics.recordTranspilation('failed', 500, { sourceFile: 'test2.cbl' });
      metrics.recordError(new Error('Test error'));
      metrics.recordCustomMetric('test.metric', 123);
    });

    test('should export metrics as JSON', () => {
      const exported = metrics.getMetrics();
      
      expect(exported).toBeDefined();
      expect(exported.transpilations.total).toBe(2);
      expect(exported.transpilations.successful).toBe(1);
      expect(exported.transpilations.failed).toBe(1);
      expect(exported.errors.total).toBe(1);
      expect(exported.custom['test.metric'].value).toBe(123);
    });

    test('should export metrics as Prometheus format', () => {
      const prometheus = metrics.exportPrometheus();
      
      expect(prometheus).toContain('transpiler_total_transpilations');
      expect(prometheus).toContain('transpiler_successful_transpilations');
      expect(prometheus).toContain('transpiler_failed_transpilations');
      expect(prometheus).toContain('transpiler_total_errors');
      expect(prometheus).toContain('transpiler_average_time');
    });

    test('should export system metrics in Prometheus format', () => {
      const prometheus = metrics.exportPrometheus();
      
      expect(prometheus).toContain('system_memory_usage');
      expect(prometheus).toContain('system_cpu_usage');
    });

    test('should export custom metrics in Prometheus format', () => {
      const prometheus = metrics.exportPrometheus();
      
      expect(prometheus).toContain('custom_test_metric');
      expect(prometheus).toContain('123');
    });
  });

  describe('Cleanup', () => {
    test('should clear system monitor interval', () => {
      const intervalId = metrics.systemMonitorInterval;
      
      metrics.cleanup();
      
      expect(metrics.systemMonitorInterval).toBeNull();
    });

    test('should clear active timers', () => {
      const timerId = metrics.startTimer('test-operation');
      
      metrics.cleanup();
      
      expect(metrics.activeTimers.size).toBe(0);
    });
  });

  describe('Event Emission', () => {
    test('should emit metric-updated event', (done) => {
      metrics.on('metric-updated', (data) => {
        expect(data.type).toBe('transpilation');
        expect(data.metrics).toBeDefined();
        done();
      });

      metrics.recordTranspilation('success', 1000);
    });

    test('should handle multiple event listeners', () => {
      let count = 0;
      const listener1 = () => count++;
      const listener2 = () => count++;

      metrics.on('transpilation', listener1);
      metrics.on('transpilation', listener2);

      metrics.recordTranspilation('success', 1000);

      expect(count).toBe(2);
    });
  });

  describe('Edge Cases', () => {
    test('should handle invalid timer ID', () => {
      const duration = metrics.endTimer('invalid-id');
      expect(duration).toBe(0);
    });

    test('should handle null error', () => {
      expect(() => {
        metrics.recordError(null);
      }).not.toThrow();
      
      expect(metrics.metrics.errors.total).toBe(1);
    });

    test('should handle undefined transpilation time', () => {
      metrics.recordTranspilation('success');
      
      expect(metrics.metrics.transpilations.total).toBe(1);
      expect(metrics.metrics.performance.totalTime).toBe(0);
    });

    test('should handle very large numbers', () => {
      metrics.recordTranspilation('success', Number.MAX_SAFE_INTEGER);
      
      expect(metrics.metrics.performance.totalTime).toBe(Number.MAX_SAFE_INTEGER);
    });
  });

  describe('Memory Management', () => {
    test('should not have memory leaks with many events', () => {
      const initialListeners = metrics.listenerCount('transpilation');
      
      for (let i = 0; i < 1000; i++) {
        metrics.recordTranspilation('success', 100);
      }
      
      expect(metrics.listenerCount('transpilation')).toBe(initialListeners);
    });

    test('should limit recent errors array size', () => {
      for (let i = 0; i < 200; i++) {
        metrics.recordError(new Error(`Error ${i}`));
      }
      
      expect(metrics.metrics.errors.recent.length).toBeLessThanOrEqual(100);
    });
  });

  describe('Concurrency', () => {
    test('should handle concurrent transpilation records', async () => {
      const promises = [];
      
      for (let i = 0; i < 100; i++) {
        promises.push(Promise.resolve().then(() => {
          metrics.recordTranspilation('success', 100);
        }));
      }
      
      await Promise.all(promises);
      
      expect(metrics.metrics.transpilations.total).toBe(100);
      expect(metrics.metrics.transpilations.successful).toBe(100);
    });

    test('should handle concurrent timer operations', async () => {
      const timerPromises = [];
      
      for (let i = 0; i < 50; i++) {
        timerPromises.push(Promise.resolve().then(() => {
          const timerId = metrics.startTimer(`operation-${i}`);
          return new Promise(resolve => {
            setTimeout(() => {
              const duration = metrics.endTimer(timerId);
              resolve(duration);
            }, 10);
          });
        }));
      }
      
      const durations = await Promise.all(timerPromises);
      
      expect(durations).toHaveLength(50);
      expect(durations.every(d => d > 0)).toBe(true);
      expect(metrics.activeTimers.size).toBe(0);
    });
  });
});