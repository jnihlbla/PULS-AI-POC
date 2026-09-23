000100 01  W27140.                                                              
000200*                                 UTDRAG UR WDK7 OCH WDL7                 
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 TIFINLV              PIC S9(5)           COMP-3.                  
001100*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
001200*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
001300     03 FLARTREG             PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500*                                 GENERAL FLAG                            
001600     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001700*                                 ARTIKELSTANDARDPRIS                     
001800*                                 STANDARD PRICE                          
001900     03 RULL-OT              OCCURS 53 TIMES.                             
002000        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
002100*                                 ANTAL ORDERTRÄFF                        
002200*                                 NO OF ORDERHITS                         
002300     03 INNEV-OT             OCCURS 5 TIMES.                              
002400        05 KVOT-INNEV        PIC S9(7)           COMP-3.                  
002500*                                 ANTAL ORDERTRÄFF                        
002600*                                 NO OF ORDERHITS                         
002700*** END OF VILMAII-COPY LENGTH= 248 BYTES                                 
