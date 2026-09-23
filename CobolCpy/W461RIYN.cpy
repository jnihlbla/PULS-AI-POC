000100 01  RIY-W461RIYN-CTX.                                                    
000200*                                 TRANSACTION FOR ORDER FROM VR           
000300*                                 TO VOLVO PARTS SYSTEM                   
000400*                                 RECORD TYPE RIY (TRAILER)               
000500     03 RIY-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RIY-IDDC             PIC X(2).                                    
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 RIY-IDARTNR          PIC 9(9).                                    
001000*                                 PART NUMBER                             
001100     03 RIY-REKSIFFR         PIC 9.                                       
001200*                                 PART NO CHECK DIGIT                     
001300     03 RIY-KVBEART          PIC 9(6).                                    
001400*                                 ORDERED QUANTITY                        
001500     03 RIY-PRARTNTO         PIC 9(7)V9(2).                               
001600*                                 NET PRICE EACH   (FOB NET)              
001700     03 RIY-PRARTBTO-EXP     PIC 9(7)V9(2).                               
001800*                                 GROSS-PRICE EXPORT                      
001900*                                  (FOB-GROSS)                            
002000     03 RIY-BERADREF         PIC X(10).                                   
002100*                                 CUSTOMERS ITEM REF.                     
002200     03 RIY-KDDSP            PIC 9.                                       
002300*                                 AFFECT ON DSP                           
002400     03 RIY-KDPRODSL         PIC 9(2).                                    
002500*                                 PRODUCT GROUP                           
002600     03 RIY-IDFKNGRP         PIC 9(4).                                    
002700*                                 FUNCTION GROUP                          
002800     03 RIY-FLINVEST         PIC X.                                       
002900*                                 EXCHANGE INVESTMENT FLAG                
003000     03 RIY-FLPRTILL         PIC X.                                       
003100*                                 PRICE PENALTY FLAG                      
003200     03 RIY-FLABON           PIC X.                                       
003300*                                 SUBSCRIBTION FLAG                       
003400     03 RIY-KDTPOTYP         PIC 9.                                       
003500*                                 TYPE OF TIME PLANNED ORDER              
003600     03 RIY-FILLERX20        PIC X(20).                                   
003700*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
