000100 01  3148-WDGX3148.                                                       
000200*                                 CORE SHIPPING DOC                       
000300*                                 USER AND DC                             
000400*                                 FYSISK NYCKEL: KY3148                   
000500*                                 (IDUSER + IDDC + IDDC-REC)              
000600     03 3148-IDUSER          PIC X(8).                                    
000700*                                 ANVÄNDARENS SÄKERHETS ID                
000800*                                 USER SECURITY-IDENTITY                  
000900     03 3148-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 3148-IDDC-REC        PIC X(2).                                    
001300*                                 MOTTAGANDE LAGER                        
001400*                                 RECEIVING WAREHOUSE                     
001500     03 3148-FLKLAR          PIC X.                                       
001600*                                 AVSLUTNINGSMARKERING                    
001700*                                 FINISHED FLAG                           
001800     03 3148-VKORDBTO        PIC S9(6)V9(1)      COMP-3.                  
001900*                                 ORDERVIKT BRUTTO (KG)                   
002000*                                 GROSS WEIGHT (KG)                       
002100     03 3148-VLORDBTO        PIC S9(4)V9(3)      COMP-3.                  
002200*                                 ORDERVOLYM BRUTTO (M3)                  
002300*                                 GROSS VOLUME PER ORDER (M3)             
002400     03 3148-TIREGDAT        PIC S9(7)           COMP-3.                  
002500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002600*                                 REGISTRATION DATE (YYMMDD)              
002700     03 3148-TIKLOCK         PIC S9(9)           COMP-3.                  
002800*                                 KLOCKSLAG (TTMMSSTH)                    
002900*                                 TIME OF DAY (HHMMSSTH)                  
003000*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
