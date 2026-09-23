000100 01  MOD-W2O36401.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDARTNR-IN       PIC X(9).                                    
000700*                                 PART NUMBER                             
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 PART NUMBER                             
001200     03 MOD-IDDC-UT          PIC X(2).                                    
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 MOD-BEART-ENG        PIC X(25).                                   
001500     03 MOD-IDLEVNR-SLAG     PIC X(5).                                    
001600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001700     03 MOD-IDLEVNR-XLAG     PIC X(5).                                    
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900     03 MOD-TILEVDAT-XLAG    PIC 9(6).                                    
002000*                                 DATE FOR FUTURE SUPPLIER                
002100     03 MOD-FLORDSP          PIC X.                                       
002200*                                 ORDER BLOCKED                           
002300     03 MOD-DAORDSP          PIC 9(6).                                    
002400*                                 STATES THE DATE WHEN FLAG OF BL         
002500*                                 OCK ORDER IS CHANGED                    
002600     03 MOD-IDUSER-ORDSP     PIC X(8).                                    
002700*                                 USER ID ORDER BLOCK                     
002800     03 MOD-FLSPBULK         PIC X.                                       
002900*                                 FLAG BULKORDER STOP                     
003000     03 MOD-DASPBULK         PIC 9(6).                                    
003100*                                 STATES THE DATE WHEN FLAG OF BL         
003200*                                 OCK BULKORDER IS CHANGED                
003300     03 MOD-IDUSER-SPBULK    PIC X(8).                                    
003400*                                 USER ID BULKORDER BLOCK                 
003500     03 MOD-KVDAGAR-MANLT    PIC Z(2)9.                                   
003600     03 MOD-IDPERSON-BUY     PIC Z(2)9.                                   
003700*                                 REFILL RESPONSIBLE ID                   
003800     03 MOD-FLREFERAL        PIC X.                                       
003900*                                 REFERALPART IN COUNTRY                  
004000     03 MOD-IDUSER-REFERAL   PIC X(8).                                    
004100*                                 USER SECURITY-IDENTITY                  
004200     03 MOD-TIREGDAT-REFERAL PIC Z(6).                                    
004300*                                 REGISTRATION DATE (YYMMDD)              
004400     03 MOD-IDLEVNR-SLAG-IN-ATTR                                          
004500                             PIC X(2).                                    
004600     03 MOD-IDLEVNR-SLAG-IN  PIC X(2).                                    
004700*                                 MFS DISPOSITION OF INPUT FIELD          
004800     03 MOD-FLORDSP-IN-ATTR  PIC X(2).                                    
004900     03 MOD-FLORDSP-IN       PIC X(2).                                    
005000*                                 MFS DISPOSITION OF INPUT FIELD          
005100     03 MOD-FLSPBULK-IN-ATTR PIC X(2).                                    
005200     03 MOD-FLSPBULK-IN      PIC X(2).                                    
005300*                                 MFS DISPOSITION OF INPUT FIELD          
005400     03 MOD-KVDAGAR-MANLT-IN-ATTR                                         
005500                             PIC X(2).                                    
005600     03 MOD-KVDAGAR-MANLT-IN PIC Z(2)9.                                   
005700     03 MOD-IDPERSON-BUY-IN-ATTR                                          
005800                             PIC X(2).                                    
005900     03 MOD-IDPERSON-BUY-IN  PIC X(2).                                    
006000*                                 MFS DISPOSITION OF INPUT FIELD          
006100     03 MOD-FLREFERAL-IN-ATTR                                             
006200                             PIC X(2).                                    
006300     03 MOD-FLREFERAL-IN     PIC X(2).                                    
006400*                                 MFS DISPOSITION OF INPUT FIELD          
006500     03 MOD-TEARTNOT-ORDER-IN-ATTR                                        
006600                             PIC X(2).                                    
006700     03 MOD-TEARTNOT-ORDER-IN                                             
006800                             PIC X(70).                                   
006900*                                 PART REMARKS NOTE ORDER                 
007000     03 MOD-TEMFSINF         PIC X(55).                                   
007100*                                 INFORMATION MESSAGE                     
007200*** END OF VILMAII-COPY LENGTH= 310 BYTES                                 
