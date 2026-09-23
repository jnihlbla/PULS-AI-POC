000100 01  RHN-W460RHN-CTX.                                                     
000200*                                 ORDER CONF TRANS. FROM VIPS             
000300*                                  TO NOAC   RECORD TYPE RHN              
000400     03 RHN-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RHN-IDDISTR          PIC 9(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 RHN-IDKUNDNR         PIC 9(6).                                    
000900*                                 CUSTOMER NO                             
001000     03 RHN-IDORDNR          PIC 9(7).                                    
001100*                                 ORDER NUMBER        IDORDNR-002         
001200     03 RHN-IDARTNR          PIC 9(9).                                    
001300*                                 PART NUMBER                             
001400     03 RHN-REKSIFFR         PIC 9.                                       
001500*                                 PART NO CHECK DIGIT                     
001600     03 RHN-KDORDBEK         PIC 9(2).                                    
001700*                                 ORDERCONFIMATIONCODE                    
001800     03 RHN-BEART            PIC X(25).                                   
001900*                                 PART DESCRIPTION                        
002000     03 RHN-KVBEART          PIC 9(6).                                    
002100*                                 ORDERED QUANTITY                        
002200     03 RHN-KDORDKL          PIC 9.                                       
002300*                                 ORDER CLASS                             
002400     03 RHN-IDLOPNR          PIC 9(3).                                    
002500*                                 SEQUENCE NUMBER                         
002600     03 RHN-IDSEKVNR         PIC 9(3).                                    
002700*                                 GENERAL SEQUENCE NUMBER                 
002800     03 RHN-FILLERX10        PIC X(10).                                   
002900*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
