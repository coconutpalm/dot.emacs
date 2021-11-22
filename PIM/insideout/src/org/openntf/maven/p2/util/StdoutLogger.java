package org.openntf.maven.p2.util;

import org.eclipse.aether.spi.log.Logger;

public class StdoutLogger {

    public static Logger log = new Logger() {

        @Override
        public boolean isDebugEnabled() {
            return true;
        }

        @Override
        public void debug(String msg) {
            System.out.println("DEBUG: " + msg);
        }

        @Override
        public void debug(String msg, Throwable error) {
            System.out.println("DEBUG: " + msg);
            error.printStackTrace();
        }

        @Override
        public boolean isWarnEnabled() {
            return true;
        }

        @Override
        public void warn(String msg) {
            System.out.println("WARN: " + msg);
        }

        @Override
        public void warn(String msg, Throwable error) {
            System.out.println("WARN: " + msg);
            error.printStackTrace();
        }
    };

}
