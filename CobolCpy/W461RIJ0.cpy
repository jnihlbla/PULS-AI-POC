000100 01  RIJ-W461RIJ0.                                                        
000200*                                 SERVICE DEGREE TO IMPORTER              
000300*                                 RECORD TYPE RIJ                         
000400     03 RIJ-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RIJ-KDCLAGER         PIC 9.                                       
000700*                                 CENTRAL WAREHOUSE CODE                  
000800     03 RIJ-IDDISTR          PIC 9(4).                                    
000900*                                 DISTRICT NUMBER                         
001000     03 RIJ-IDKUNDNR         PIC 9(6).                                    
001100*                                 CUSTOMER NO                             
001200     03 RIJ-IDORDNR          PIC 9(7).                                    
001300*                                 ORDER NUMBER        IDORDNR-002         
001400     03 RIJ-KDORDKL          PIC 9.                                       
001500*                                 ORDER CLASS                             
001600     03 RIJ-IDARTNR          PIC 9(9).                                    
001700*                                 PART NUMBER                             
001800     03 RIJ-REKSIFFR         PIC 9.                                       
001900*                                 PART NO CHECK DIGIT                     
002000     03 RIJ-KVBEART          PIC 9(6).                                    
002100*                                 ORDERED QUANTITY                        
002200     03 RIJ-FLIHOP           PIC X.                                       
002300     03 FILLER               PIC X(41).                                   
002400*** END COPY W461RIJ0C0  LENGTH=80                                        
