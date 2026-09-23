000100 01  TOP-WDM315.                                                          
000200*                                 URVALSREGISTER                          
000300*                                 ARTIKELSTATISTIK                        
000400*                                 TOP SALES SEGMENT                       
000500*                                 FYSISK-NYCKEL: KDSEGKEY                 
000600     03 TOP-KDSEGKEY         PIC X.                                       
000700*                                 TEKNISK SEGMENT-NYCKEL                  
000800*                                 TECHNICAL SEGMENT KEY                   
000900     03 TOP-KVART            PIC S9(7)           COMP-3.                  
001000*                                 ANTAL ARTNR PER BRYTBEGREPP             
001100*                                 NO OF PARTNOS PER TYPE                  
001200     03 TOP-KDPRODSL         OCCURS 4 TIMES                               
001300                             PIC S9(3)           COMP-3.                  
001400*                                 PRODUKTSLAG                             
001500*                                 PRODUCT GROUP                           
001600     03 TOP-IDFKNGRP-FOM     PIC S9(5)           COMP-3.                  
001700*                                 FUNKTIONSGRUPP                          
001800*                                 FUNCTION GROUP                          
001900     03 TOP-IDFKNGRP-TOM     PIC S9(5)           COMP-3.                  
002000*                                 FUNKTIONSGRUPP                          
002100*                                 FUNCTION GROUP                          
002200     03 TOP-MARKN-GRP        OCCURS 4 TIMES.                              
002300*                                 GRUPPNIVÅ MARKNAD                       
002400        05 TOP-KDMARK-FOM    PIC S9(3)           COMP-3.                  
002500*                                 MARKNADSKOD                             
002600*                                 MARKET CODE                             
002700        05 TOP-KDMARK-TOM    PIC S9(3)           COMP-3.                  
002800*                                 MARKNADSKOD                             
002900*                                 MARKET CODE                             
003000     03 TOP-IDKONCNR         OCCURS 8 TIMES                               
003100                             PIC S9(3)           COMP-3.                  
003200*                                 KONCERNNUMMER                           
003300*                                 CONCERN NO                              
003400     03 TOP-IDLEVNR          OCCURS 8 TIMES                               
003500                             PIC X(5).                                    
003600*                                 LEVERANTÖRNUMMER                        
003700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003800     03 TOP-DISTR-GRP        OCCURS 4 TIMES.                              
003900*                                 GRUPPNIVÅ DISTRIKT                      
004000        05 TOP-IDDISTR-FOM   PIC S9(5)           COMP-3.                  
004100*                                 DISTRIKTNUMMER                          
004200*                                 DISTRICT NUMBER                         
004300        05 TOP-IDDISTR-TOM   PIC S9(5)           COMP-3.                  
004400*                                 DISTRIKTNUMMER                          
004500*                                 DISTRICT NUMBER                         
004600     03 TOP-IDPTYP           PIC X(3).                                    
004700*                                 POSTTYP                                 
004800*                                 RECORD TYPE                             
004900     03 TOP-ANSK-GRP         OCCURS 4 TIMES.                              
005000*                                 GRUPPNIVÅ ANSKAFFARE                    
005100        05 TOP-IDANSK-FOM    PIC S9(3)           COMP-3.                  
005200*                                 ANSKAFFARNUMMER                         
005300*                                 PROCURER NO.                            
005400        05 TOP-IDANSK-TOM    PIC S9(3)           COMP-3.                  
005500*                                 ANSKAFFARNUMMER                         
005600*                                 PROCURER NO.                            
005700     03 TOP-FILLER           PIC X(6).                                    
005800*** END OF VILMAII-COPY LENGTH= 140 BYTES                                 
