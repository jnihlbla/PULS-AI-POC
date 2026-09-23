000100 01  W2O11201.                                                            
000200*                                 COPYTEXT FÖR MID W2O11201               
000300     03 TRANS-NUMMER         PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MESSAGE              PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 IDLEVNR-IN           PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 IDLEVNR-UT           PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 ANT                  PIC 9(3).                                    
001200*                                 ANTAL SKRIVNA RABATTSEGMENT             
001300     03 IDFTG-UT             PIC 9(2).                                    
001400*                                 FÖRETAGSID EKONOM REDOVISNING           
001500     03 AREA.                                                             
001600        05 BELEV             PIC X(35).                                   
001700*                                 LEVERANTÖRSNAMN                         
001800        05 KDVALLEV          PIC Z(2)9.                                   
001900*                                 VALUTAKOD LEVERANTÖR                    
002000        05 RETULF-1          PIC Z(2)9.9(4).                              
002100*                                 TULLFAKTOR                              
002200        05 RETULF-2          PIC Z(5)9.9(4).                              
002300        05 TITULF            PIC X(7).                                    
002400*                                 TILLÄMPNINGSDATUM FÖR                   
002500*                                 TULLFAKTOR      (ÅÅMMDD)                
002600        05 KDVALISO          PIC X(3).                                    
002700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002800        05 RABRADER          OCCURS 10 TIMES                              
002900                             INDEXED RAB-INDX.                            
003000           07 KDRAB          PIC X(5).                                    
003100*                                 RABATTKOD                               
003200           07 RERAB          PIC Z(2)9.9.                                 
003300*                                 RABATTSATS (PROCENT)                    
003400        05 LARMGRRADER       OCCURS 10 TIMES                              
003500                             INDEXED LARMGR-INDX.                         
003600           07 KDPRODGRP      PIC X.                                       
003700*                                 PRODUKTGRUPP R,B,T,V ELLER D            
003800           07 KRLARMG        PIC Z(8)9.                                   
003900           07 RELARMG        PIC Z(2)9.9.                                 
004000*                                 LARMGRÄNS PROCENT                       
004100        05 LINE23.                                                        
004200           07 MESSAGE-BOTTOM PIC X(80).                                   
004300*                                 MEDDELANDEFÄLT PÅ RAD 23                
004400*** END OF VILMAII-COPY LENGTH= 456 BYTES                                 
