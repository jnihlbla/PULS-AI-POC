000100 01  RKG-W461RKG0-CTX.                                                    
000200*                                 EXCHANGE  FOLLOW-UP                     
000300*                                 RECORD TYP  RKG                         
000400     03 RKG-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RKG-IDDISTR          PIC 9(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 RKG-IDKUNDNR         PIC 9(6).                                    
000900*                                 CUSTOMER NO                             
001000     03 RKG-IDBYTRAP         PIC 9(7).                                    
001100*                                 REPORTNUMBER   EXCHANGE                 
001200     03 RKG-TIREGDAT-GODK    PIC 9(6).                                    
001300*                                 REGISTRATION DATE (YYMMDD)              
001400     03 RKG-IDARTNR-OBJ      PIC 9(9).                                    
001500*                                 PART NUMBER                             
001600     03 RKG-IDTABNR          PIC 9(3).                                    
001700*                                 TABELNUMBER                             
001800     03 RKG-KVRETUR-GODK     PIC 9(5).                                    
001900*                                 NUMBER IN RETURN                        
002000     03 RKG-KDBYTSTA-OBJ     PIC X.                                       
002100*                                 STATUSCODE EXCH CORES                   
002200     03 RKG-IDORDNR          PIC 9(7).                                    
002300*                                 ORDER NUMBER        IDORDNR-002         
002400     03 RKG-KDBYTREF         PIC X(3).                                    
002500*                                 CENTRAL REFERENCE                       
002600     03 RKG-IDKUNDRF         PIC X(10).                                   
002700*                                 CUSTOMER REFERENCE (ORDER ID)           
002800     03 RKG-IDBYTRAD         PIC 9(4).                                    
002900*                                 LINE NO                                 
003000     03 RKG-FILLERX12        PIC X(12).                                   
003100*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
