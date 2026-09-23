000100 01  W155001.                                                             
000200*                                 COPYTEXT FÖR HISTORIKREG.               
000300*                                 HUVUDPOST                               
000400*                                 IDPTYP      131/155                     
000500*                                 IDPTYP-1    001                         
000600*                                 IDKATGRP    ZERO                        
000700*                                 IDKATAVS    0                           
000800*                                 IDKATRAD    0                           
000900*                                                                         
001000     03 IDKATNR              PIC S9(5)           COMP-3.                  
001100*                                 KATALOG-ID                              
001200     03 IDKATGRP             PIC X(3).                                    
001300*                                 KATALOG-GRUPP                           
001400     03 IDKATAVS             PIC S9(5)           COMP-3.                  
001500*                                 KATALOG-AVSNITT                         
001600     03 IDKATRAD             PIC S9(5)           COMP-3.                  
001700*                                 RADNUMMER                               
001800     03 IDPTYP               PIC X(3).                                    
001900*                                 POSTTYP                                 
002000     03 IDPTYP-1             PIC X(3).                                    
002100*                                 POSTTYP                                 
002200     03 KDFORDON             PIC X(2).                                    
002300*                                 FORDONSSLAG                             
002400     03 BEEMBLEM             PIC X(5).                                    
002500*                                 EMBLEM                                  
002600     03 BEMASTER             PIC X(12).                                   
002700*                                 MASTERNAMN                              
002800     03 BEKAT-1              PIC X(40).                                   
002900*                                 KATALOGBETECKNING                       
003000*                                 SE ÄVEN BEEMBLEM RESP BEMASTER          
003100     03 BEKAT-2              PIC X(20).                                   
003200*                                 KATALOGBETECKNING                       
003300*                                 SE ÄVEN BEEMBLEM RESP BEMASTER          
003400     03 TIREGDAT             PIC S9(7)           COMP-3.                  
003500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003600     03 TIOMBRYT             PIC S9(7)           COMP-3.                  
003700*                                 OMBRYTNINGSDATUM                        
003800     03 TIOMBRYT-1           PIC S9(7)           COMP-3.                  
003900*                                 OMBRYTNINGSDATUM                        
004000     03 TIHIST               PIC S9(7)           COMP-3.                  
004100*                                 FLYTTNINGSDATUM                         
004200     03 IDVERS               PIC S9(3)           COMP-3.                  
004300*                                 UTGÅVA                                  
004400     03 FLKOPIE              PIC X.                                       
004500*                                 TILLÅTEN ATT LÅNA HÄRIFRÅN              
004600     03 TENOTE               PIC X(40).                                   
004700*                                 NOTERING                                
004800*** END COPY W155001CC0  LENGTH=156                                       
