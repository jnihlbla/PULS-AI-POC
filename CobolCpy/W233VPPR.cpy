000010*** EDIT ALLOWED                                                          
000100 01  W233VPPR.                                                            
000200*                      COPYTEXT FIL W23356 TILL INKÖP NEDCAR              
000300*                                          TÖMNING KÖPTRANSAR             
000400*                                    ********                             
000500*                                    * VPPR *                             
000510*                                    ********                             
000600    03  FZ2RECT            PIC X(4).                                      
000700*                                   RECORD TYPE 'VPPR'                    
000710    03  FZ2FUNC            PIC X.                                         
000720*                                   FUNCTION N=NEW C=CHANGE               
000730*                                            D=DELEATE                    
000800    03  FZ2PART-7.                                                        
000900      05  IDARTNR-7        PIC 9(7).                                      
001100      05  FILLER           PIC X(5).                                      
001200*                                    PART NUMBER                          
001210    03  FZ2PART-8 REDEFINES FZ2PART-7.                                    
001211      05  IDARTNR-8        PIC 9(8).                                      
001213      05  FILLER           PIC X(4).                                      
001214*                                    PART NUMBER                          
001220    03  FZ2DATE            PIC 9(6).                                      
001230*                                    DATE YYMMDD                          
001260    03  FZ2ESYQ            PIC  9(8).                                     
001270*                                    ESTIMATED YEAR QTY                   
001700    03  FZ2DELQ            PIC  9(8).                                     
001800*                                    QTY FIRST DELIVERY                   
002100    03  FZ2DELD            PIC 9(6).                                      
002200*                                    DATE FIRST DELIVERY YYMMDD           
002600    03  FZ2FILLER          PIC X(19).                                     
009800*                                                                         
009900***END COPY W233VPPR LENGTH=64                                            
