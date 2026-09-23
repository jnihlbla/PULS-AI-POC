000100 01  RKA-W461RKA0-CTX.                                                    
000200*                                 DESCRIPTION TRAN TO                     
000300*                                 IMPORTERS   RECTYPE RKA                 
000400     03 RKA-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RKA-BEART            OCCURS 2 TIMES                               
000700                             PIC X(25).                                   
000800*                                 PART DESCRIPTION                        
000900     03 RKA-KDRABATT         PIC 9(3).                                    
001000*                                 PURCHASE DISCOUNT CODE                  
001100     03 RKA-FILLERX24        PIC X(24).                                   
001200*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
