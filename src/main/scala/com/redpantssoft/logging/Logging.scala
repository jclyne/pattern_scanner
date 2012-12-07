package com.redpantssoft.logging

import org.slf4j.{Logger, LoggerFactory}

/** Trait that enables logging
  *
  * This trait implements a wrapper for "Simple Logging Facade
  * for Java" (slf4j). Uses by-name parameters to delay msg
  * evaluation until after the log level is checked.
  *
  * Use:
  * new MyClass extends Logging
  * trace("Hello World!");
  * debug("How are you today?");
  * info("I am fine.");
  * warn("I love programming.");
  * error("I am programming.");
  */

trait Logging {

  /** The name of this logger */
  var loggerName: String = this.getClass.getName

  /** The wrapped SLF4J logger */
  lazy val logger: Logger = LoggerFactory.getLogger(loggerName)

  /** Log a message with ERROR level.
    * @param msg The message to be logged
    */
  def error(msg: => String) {
    if (logger.isErrorEnabled && msg.size > 0)
      logger.error(msg)
  }

  /** Log a message with ERROR level.
    * @param msg The message to be logged
    * @param t The Throwable to be logged
    */
  def error(msg: => String, t: Throwable) {
    if (logger.isErrorEnabled && msg.size > 0)
      logger.error(msg, t)
  }

  /** Log a message with WARN level.
    * @param msg The message to be logged
    */
  def warn(msg: => String) {
    if (logger.isWarnEnabled && msg.size > 0)
      logger.warn(msg)
  }

  /** Log a message with WARN level.
    * @param msg The message to be logged
    * @param t The Throwable to be logged
    */
  def warn(msg: => String, t: Throwable) {
    if (logger.isWarnEnabled && msg.size > 0)
      logger.warn(msg, t)
  }

  /** Log a message with INFO level.
    * @param msg The message to be logged
    */
  def info(msg: => String) {
    if (logger.isInfoEnabled && msg.size > 0)
      logger.info(msg)
  }

  /** Log a message with INFO level.
    * @param msg The message to be logged
    * @param t The Throwable to be logged
    */
  def info(msg: => String, t: Throwable) {
    if (logger.isInfoEnabled && msg.size > 0)
      logger.info(msg, t)
  }

  /** Log a message with DEBUG level.
    * @param msg The message to be logged
    */
  def debug(msg: => String) {
    if (logger.isDebugEnabled && msg.size > 0)
      logger.debug(msg)
  }

  /** Log a message with DEBUG level.
    * @param msg The message to be logged
    * @param t The Throwable to be logged
    */
  def debug(msg: => String, t: Throwable) {
    if (logger.isDebugEnabled && msg.size > 0)
      logger.debug(msg, t)
  }

  /** Log a message with TRACE level.
    * @param msg The message to be logged
    */
  def trace(msg: => String) {
    if (logger.isTraceEnabled && msg.size > 0)
      logger.trace(msg)
  }

  /** Log a message with TRACE level.
    * @param msg The message to be logged
    * @param t The Throwable to be logged
    */
  def trace(msg: => String, t: Throwable) {
    if (logger.isTraceEnabled && msg.size > 0)
      logger.trace(msg, t)
  }

}