object Form1: TForm1
  Left = 936
  Top = 153
  Width = 331
  Height = 142
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object st_prompt: TLabel
    Left = 24
    Top = 64
    Width = 97
    Height = 25
    AutoSize = False
  end
  object sle_filename: TEdit
    Left = 24
    Top = 24
    Width = 241
    Height = 21
    TabOrder = 0
  end
  object cb_convert: TButton
    Left = 136
    Top = 64
    Width = 75
    Height = 25
    Caption = #36716#25442
    TabOrder = 1
    OnClick = cb_convertClick
  end
  object cb_exit: TButton
    Left = 224
    Top = 64
    Width = 75
    Height = 25
    Caption = #36864#20986
    TabOrder = 2
    OnClick = cb_exitClick
  end
  object cb_browse: TButton
    Left = 272
    Top = 24
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 3
    OnClick = cb_browseClick
  end
end
