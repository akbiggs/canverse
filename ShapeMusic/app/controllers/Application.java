package controllers;

import play.*;
import play.mvc.*;

import views.html.*;

public class Application extends Controller {

    public static Result index() {
        return ok(index.render());
    }

	public static Result about(){
		return ok (about.render());
	}

	public static Result documents(){
		return ok (documents.render());
	}
	
	public static Result group(){
		return ok (group.render());
	}
}
