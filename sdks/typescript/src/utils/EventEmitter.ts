/**
 * Event emitter utility for LegacyBAAS SDK
 */

export type EventListener = (...args: any[]) => void;

export class EventEmitter {
  private events: Map<string, EventListener[]> = new Map();
  private maxListeners: number = 10;

  /**
   * Add event listener
   */
  on(event: string, listener: EventListener): this {
    if (!this.events.has(event)) {
      this.events.set(event, []);
    }
    
    const listeners = this.events.get(event)!;
    
    if (listeners.length >= this.maxListeners) {
      console.warn(
        `MaxListenersExceededWarning: Possible EventEmitter memory leak detected. ` +
        `${listeners.length + 1} ${event} listeners added. ` +
        `Use emitter.setMaxListeners() to increase limit`
      );
    }
    
    listeners.push(listener);
    return this;
  }

  /**
   * Add one-time event listener
   */
  once(event: string, listener: EventListener): this {
    const onceWrapper = (...args: any[]) => {
      this.off(event, onceWrapper);
      listener(...args);
    };
    
    return this.on(event, onceWrapper);
  }

  /**
   * Remove event listener
   */
  off(event: string, listener: EventListener): this {
    const listeners = this.events.get(event);
    if (!listeners) {
      return this;
    }
    
    const index = listeners.indexOf(listener);
    if (index > -1) {
      listeners.splice(index, 1);
    }
    
    if (listeners.length === 0) {
      this.events.delete(event);
    }
    
    return this;
  }

  /**
   * Remove all listeners for an event
   */
  removeAllListeners(event?: string): this {
    if (event) {
      this.events.delete(event);
    } else {
      this.events.clear();
    }
    
    return this;
  }

  /**
   * Emit event
   */
  emit(event: string, ...args: any[]): boolean {
    const listeners = this.events.get(event);
    if (!listeners || listeners.length === 0) {
      return false;
    }
    
    // Create a copy to avoid issues with listeners that remove themselves
    const listenersCopy = [...listeners];
    
    for (const listener of listenersCopy) {
      try {
        listener(...args);
      } catch (error) {
        console.error(`Error in event listener for '${event}':`, error);
      }
    }
    
    return true;
  }

  /**
   * Get listeners for an event
   */
  listeners(event: string): EventListener[] {
    return [...(this.events.get(event) || [])];
  }

  /**
   * Get listener count for an event
   */
  listenerCount(event: string): number {
    return this.events.get(event)?.length || 0;
  }

  /**
   * Get all event names
   */
  eventNames(): string[] {
    return Array.from(this.events.keys());
  }

  /**
   * Set maximum number of listeners per event
   */
  setMaxListeners(n: number): this {
    this.maxListeners = n;
    return this;
  }

  /**
   * Get maximum number of listeners per event
   */
  getMaxListeners(): number {
    return this.maxListeners;
  }

  /**
   * Add listener to beginning of listeners array
   */
  prependListener(event: string, listener: EventListener): this {
    if (!this.events.has(event)) {
      this.events.set(event, []);
    }
    
    this.events.get(event)!.unshift(listener);
    return this;
  }

  /**
   * Add one-time listener to beginning of listeners array
   */
  prependOnceListener(event: string, listener: EventListener): this {
    const onceWrapper = (...args: any[]) => {
      this.off(event, onceWrapper);
      listener(...args);
    };
    
    return this.prependListener(event, onceWrapper);
  }
}