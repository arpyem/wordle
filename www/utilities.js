let guess_status_1 = [];
let guess_status_2 = [];
let guess_status_3 = [];
let guess_status_4 = [];
let guess_status_5 = [];

let Letter = class {
    constructor(id = '', position = 0, status = 's1') {
        this.id = id;
        this.position = position;
  	    this.status = status;
    }
    updateStatus() {
  	    if (this.status == 's1') {
    	    this.status = 's2';
        } else if (this.status == 's2') {
    	    this.status = 's3';
        } else if (this.status == 's3') {
    	    this.status = 's1';
        } else {
    	    this.status = 's1';
        }
    }
    get new_status() {
        return this.updateStatus();
    }
};

function update_status(Letter, word, inputId) {
    Letter.new_status;
    var id = Letter.id;
    var element = document.querySelector('#' + id);
    var status = element.className.split(' ')[1];
    element.classList.remove(status);
    element.classList.add(Letter.status);
    word[Letter.position] = Letter.status;
    Shiny.setInputValue(inputId, word, {priority: 'event'});
}