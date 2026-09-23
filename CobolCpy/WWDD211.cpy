000100 01  ART-WWDD211.                                                         
000200*                                 COPYTEXT TILL W.NYPONP.WWDD2            
000300*                                 SEKVENSFIL AV WDD2                      
000400*                                 BASLAGERINFORMATION                     
000500*                                    PTYP = 2                             
000600*                                 SORTAT PÅ IDARTNR-SORT +KDBASLM         
000700     03 ART-POSTTYP          PIC X.                                       
000800     03 ART-IDARTNR-SORT     PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 ART-KDBASLM          PIC X(6).                                    
001200*                                 BASLAGERMARKNAD                         
001300*                                 BASIC STOCK MARKET                      
001400     03 ART-IDDISTR          OCCURS 6 TIMES                               
001500                             PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700*                                 DISTRICT NUMBER                         
001800     03 ART-IDFKNGRP         PIC S9(5)           COMP-3.                  
001900*                                 FUNKTIONSGRUPP                          
002000*                                 FUNCTION GROUP                          
002100     03 ART-IDPROJ           PIC X(4).                                    
002200*                                 PARTS PROJEKTIDENTITET                  
002300*                                 PARTS PROJECT IDENTITY                  
002400     03 ART-KDBPSR           PIC S9              COMP-3.                  
002500*                                 BASLAGERFÖRSLAGSNIVÅ                    
002600*                                 BASIC PART STOCK RECOMMENDATION         
002700     03 ART-KDDEALER         PIC X.                                       
002800*                                 BASLAGER KUNDKOD                        
002900*                                 BASIC STOCK DEALER CODE                 
003000     03 ART-KVBASLKIT        PIC S9(7)           COMP-3.                  
003100*                                 ANTAL I ÅTERFÖLSÄLJARSATS               
003200*                                 DEALERKIT QUANTITY                      
003300     03 ART-KVBASLM          PIC S9(7)           COMP-3.                  
003400*                                 BASLAGER TOTAL PER MARKNAD              
003500*                                 BASIC STOCK PER MARKET                  
003600     03 ART-KVBASLMD         OCCURS 6 TIMES                               
003700                             PIC S9(7)           COMP-3.                  
003800*                                 BASLAGER DISTRIKT KVANTITET             
003900*                                 BASIC STOCK DISTRICT QUANTITY           
004000     03 ART-TEARTNOT-MARK    PIC X(40).                                   
004100*                                 BASLAGERMARKNAD ARTIKEL NOTE.           
004200*                                 PARTS NOTIFY BASIC STOCK MARKET         
004300     03 ART-TIBASLM          PIC S9(7)           COMP-3.                  
004400*                                 MARKNADSUPPDATERINGSDATUM               
004500*                                 UPDATE DAY FOR THE MARKET               
004600     03 ART-TISTOMREG        PIC S9(7)           COMP-3.                  
004700*                                 STOPPTID MARKNADSREGISTRERING           
004800*                                 STOP-TIME MARKET REGISTRATION           
004900     03 ART-FLBLMQ           PIC X.                                       
005000*                                 ARTIKEL TIDIGARE PÅ BASLAGERKÖ?         
005100*                                 PART EARLIER ON BASIC STOCK Q?          
005200*** END OF VILMAII-COPY LENGTH= 120 BYTES                                 
