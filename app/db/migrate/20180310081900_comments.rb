class Comments < ActiveRecord::Migration[5.1]
  def change
    create_table :comments do |t|
      t.belongs_to :topic
      t.string :comment
      t.timestamps
    end
  end
end
