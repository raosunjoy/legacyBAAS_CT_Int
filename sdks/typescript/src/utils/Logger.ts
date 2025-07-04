/**
 * Logger utility for LegacyBAAS SDK
 */

export type LogLevel = 'DEBUG' | 'INFO' | 'WARN' | 'ERROR';

export class Logger {
  private logLevel: LogLevel;
  private prefix: string;

  constructor(logLevel: LogLevel = 'INFO', prefix: string = 'LegacyBAAS') {
    this.logLevel = logLevel;
    this.prefix = prefix;
  }

  private getLogLevelNumber(level: LogLevel): number {
    switch (level) {
      case 'DEBUG': return 0;
      case 'INFO': return 1;
      case 'WARN': return 2;
      case 'ERROR': return 3;
      default: return 1;
    }
  }

  private shouldLog(level: LogLevel): boolean {
    return this.getLogLevelNumber(level) >= this.getLogLevelNumber(this.logLevel);
  }

  private formatMessage(level: LogLevel, message: string, data?: any): string {
    const timestamp = new Date().toISOString();
    const formattedMessage = `[${timestamp}] [${this.prefix}] [${level}] ${message}`;
    
    if (data) {
      return `${formattedMessage} ${JSON.stringify(data, null, 2)}`;
    }
    
    return formattedMessage;
  }

  debug(message: string, data?: any): void {
    if (this.shouldLog('DEBUG')) {
      console.debug(this.formatMessage('DEBUG', message, data));
    }
  }

  info(message: string, data?: any): void {
    if (this.shouldLog('INFO')) {
      console.info(this.formatMessage('INFO', message, data));
    }
  }

  warn(message: string, data?: any): void {
    if (this.shouldLog('WARN')) {
      console.warn(this.formatMessage('WARN', message, data));
    }
  }

  error(message: string, error?: any): void {
    if (this.shouldLog('ERROR')) {
      if (error instanceof Error) {
        console.error(this.formatMessage('ERROR', message, {
          name: error.name,
          message: error.message,
          stack: error.stack
        }));
      } else {
        console.error(this.formatMessage('ERROR', message, error));
      }
    }
  }

  setLogLevel(level: LogLevel): void {
    this.logLevel = level;
  }

  getLogLevel(): LogLevel {
    return this.logLevel;
  }
}