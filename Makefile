.PHONY: lint clean

clean:
	clj -T:build clean

lint:
	clj-kondo --config .clj-kondo/config.edn --lint src

flow-storm-cljs-compiler-plugin.jar:
	clj -T:build jar

install: flow-storm-cljs-compiler-plugin.jar
	mvn install:install-file -Dfile=target/flow-storm-cljs-compiler-plugin.jar -DpomFile=target/classes/META-INF/maven/com.github.flow-storm/flow-storm-cljs-compiler-plugin/pom.xml

deploy:
	mvn deploy:deploy-file -Dfile=target/flow-storm-cljs-compiler-plugin.jar -DrepositoryId=clojars -DpomFile=target/classes/META-INF/maven/com.github.flow-storm/flow-storm-cljs-compiler-plugin/pom.xml -Durl=https://clojars.org/repo


