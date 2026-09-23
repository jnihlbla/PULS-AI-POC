000100 01  RIT-W461RITN-CTX.                                                    
000200*                                 TRANSACTION FOR ORDER ENTERED           
000300*                                 DIRECT IN VOLVO PARTS SYSTEM            
000400*                                 BY-PASSING IMPORTERS SYSTEM             
000500*                                 RECORD TYPE RIT                         
000600     03 RIT-IDPTYP           PIC X(3).                                    
000700*                                 RECORD TYPE                             
000800     03 RIT-IDDISTR          PIC 9(4).                                    
000900*                                 DISTRICT NUMBER                         
001000     03 RIT-IDKUNDNR         PIC 9(6).                                    
001100*                                 CUSTOMER NO                             
001200     03 RIT-IDORDNR          PIC 9(7).                                    
001300*                                 ORDER NUMBER        IDORDNR-002         
001400     03 RIT-KDORDKL          PIC 9.                                       
001500*                                 ORDER CLASS                             
001600     03 RIT-IDARTNR          PIC 9(9).                                    
001700*                                 PART NUMBER                             
001800     03 RIT-REKSIFFR         PIC 9.                                       
001900*                                 PART NO CHECK DIGIT                     
002000     03 RIT-BERADREF         PIC X(10).                                   
002100*                                 CUSTOMERS ITEM REF.                     
002200     03 RIT-BEVOLREF         PIC X(10).                                   
002300*                                 VOLVO REFERENCE                         
002400     03 RIT-KVBEART          PIC 9(6).                                    
002500*                                 ORDERED QUANTITY                        
002600     03 RIT-KDDSP            PIC 9.                                       
002700*                                 AFFECT ON DSP                           
002800     03 RIT-FLABON           PIC X.                                       
002900*                                 SUBSCRIBTION FLAG                       
003000     03 RIT-KDTPOTYP         PIC 9.                                       
003100*                                 TYPE OF TIME PLANNED ORDER              
003200     03 RIT-FILLERX20        PIC X(20).                                   
003300*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
