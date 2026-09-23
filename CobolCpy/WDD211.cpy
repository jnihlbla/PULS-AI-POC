000100 01  ART-WDD211.                                                          
000200*                                 ARTIKELREGISTER                         
000300*                                 NYA ARTIKLAR FRÅN PV OCH LV             
000400*                                 SOM SKALL BEREDAS                       
000500*                                 BASLAGERINFORMATION                     
000600*                                 FYSISK NYCKEL: KDBASLM                  
000700     03 ART-KDBASLM          PIC X(6).                                    
000800*                                 BASLAGERMARKNAD                         
000900*                                 BASIC STOCK MARKET                      
001000     03 ART-IDDISTR          OCCURS 6 TIMES                               
001100                             PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 ART-IDFKNGRP         PIC S9(5)           COMP-3.                  
001500*                                 FUNKTIONSGRUPP                          
001600*                                 FUNCTION GROUP                          
001700     03 ART-IDPROJ           PIC X(4).                                    
001800*                                 PARTS PROJEKTIDENTITET                  
001900*                                 PARTS PROJECT IDENTITY                  
002000     03 ART-KDBPSR           PIC S9              COMP-3.                  
002100*                                 BASLAGERFÖRSLAGSNIVÅ                    
002200*                                 BASIC PART STOCK RECOMMENDATION         
002300     03 ART-KDDEALER         PIC X.                                       
002400*                                 BASLAGER KUNDKOD                        
002500*                                 BASIC STOCK DEALER CODE                 
002600     03 ART-KVBASLKIT        PIC S9(7)           COMP-3.                  
002700*                                 ANTAL I ÅTERFÖLSÄLJARSATS               
002800*                                 DEALERKIT QUANTITY                      
002900     03 ART-KVBASLM          PIC S9(7)           COMP-3.                  
003000*                                 BASLAGER TOTAL PER MARKNAD              
003100*                                 BASIC STOCK PER MARKET                  
003200     03 ART-KVBASLMD         OCCURS 6 TIMES                               
003300                             PIC S9(7)           COMP-3.                  
003400*                                 BASLAGER DISTRIKT KVANTITET             
003500*                                 BASIC STOCK DISTRICT QUANTITY           
003600     03 ART-TEARTNOT-MARK    PIC X(40).                                   
003700*                                 BASLAGERMARKNAD ARTIKEL NOTE.           
003800*                                 PARTS NOTIFY BASIC STOCK MARKET         
003900     03 ART-TIBASLM          PIC S9(7)           COMP-3.                  
004000*                                 MARKNADSUPPDATERINGSDATUM               
004100*                                 UPDATE DAY FOR THE MARKET               
004200     03 ART-TISTOMREG        PIC S9(7)           COMP-3.                  
004300*                                 STOPPTID MARKNADSREGISTRERING           
004400*                                 STOP-TIME MARKET REGISTRATION           
004500     03 ART-FLBLMQ           PIC X.                                       
004600*                                 ARTIKEL TIDIGARE PÅ BASLAGERKÖ?         
004700*                                 PART EARLIER ON BASIC STOCK Q?          
004800*** END COPY WDD211      LENGTH=114                                       
