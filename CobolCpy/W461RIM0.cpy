000100 01  RIM-W461RIM0-CTX.                                                    
000200*                                 INVOICE REFERENCE TO IMPORTER           
000300*                                  1 / ORDER IN THE INVOICE               
000400*                                 RECORD TYPE RIM                         
000500     03 RIM-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RIM-IDKUNDNR         PIC 9(6).                                    
000800*                                 CUSTOMER NO                             
000900     03 RIM-IDORDNR          PIC 9(7).                                    
001000*                                 ORDER NUMBER        IDORDNR-002         
001100     03 RIM-IDPRODNR         PIC 9(7).                                    
001200*                                 PRODUCTION NUMBER                       
001300     03 RIM-TIORDREG         PIC 9(6).                                    
001400*                                 ORDER REGISTRATION DATE  YYMMDD         
001500     03 RIM-KDORDKL          PIC 9.                                       
001600*                                 ORDER CLASS                             
001700     03 RIM-BEVOLREF         PIC X(10).                                   
001800*                                 VOLVO REFERENCE                         
001900     03 RIM-BEVARREF         PIC X(10).                                   
002000*                                 OUR REFERENCE                           
002100     03 RIM-KDREFNOT         PIC X(2).                                    
002200*                                 INVOICE NOTES                           
002300     03 RIM-KDFRAKT          PIC 9(2).                                    
002400*                                 FREIGHT CODE                            
002500     03 RIM-IDTRPBO.                                                      
002600*                                 TRANSPORT BOLLA DOCUMENT IDENT.         
002700        05 RIM-IDTRPBOT      PIC X.                                       
002800*                                 TRANSPORT BOLLA DOCUMENT LETTER         
002900        05 RIM-IDTRPBON      PIC 9(7).                                    
003000*                                 TRANSPORT BOLLA DOCUMENT NO.            
003100     03 RIM-PRFRAKT-LOC      PIC 9(7)V9(2).                               
003200*                                 FREIGHT COST LOCAL CURRENCY             
003300     03 RIM-FILLERX9         PIC X(9).                                    
003400*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
