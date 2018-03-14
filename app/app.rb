
require 'sinatra'
require 'sinatra/reloader'
require 'sinatra/activerecord'
require './models/connect.rb'
require './models/comment.rb'
require './models/topic.rb'


get '/api/topics' do
    Topic.all.to_json
end

get '/api/topic' do
    topic = Topic.find_by_id(params[:topic_id])

    if topic
        p topic
        topic.to_json
    else
        {type: "error", message: "topic was not found"}.to_json
    end
end

get '/api/comments' do
    topic_id = params[:topic_id]
    
    if topic_id
        topic = Topic.find(topic_id)

        if topic
            topic.comments.to_json
        else
            {type: "error", message: "corresponded topic does not found"}.to_json
        end

    else
        {type: "error", message: "topic_id was not set"}.to_json
    end
end

post '/api/comments/:topic_id' do
    json = JSON.parse(request.body.read)

    topic = Topic.find_by_id(params[:topic_id])

    if topic
        topic.comments.create(json)
        topic.comments.to_json
    else
        { type: "error", message: "topic does not found" }.to_json
    end 
end

post '/api/topics/create' do
    json = JSON.parse(request.body.read)

    topic = Topic.new(json)

    if topic.save
        topic.to_json
    else
        {type: "error", message: "topic creation failed"}.to_json
    end
end

not_found do
    send_file File.join(settings.public_folder, "index.html")
end
