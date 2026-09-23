000010 01  RIA-W461RIA0.                                                        
000020*                                 ATTATCHED BO TO IMPORTER                
000030*                                 RECORD TYPE RIA                         
000040     03 RIA-IDPTYP           PIC X(3).                                    
000050*                                 RECORD TYPE                             
000060     03 RIA-KDCLAGER         PIC 9.                                       
000070*                                 CENTRAL WAREHOUSE CODE                  
000080     03 RIA-IDDISTR          PIC 9(4).                                    
000090*                                 DISTRICT NUMBER                         
000100     03 RIA-IDKUNDNR         PIC 9(6).                                    
000110*                                 CUSTOMER NO                             
000120     03 RIA-IDORDNR          PIC 9(7).                                    
000130*                                 ORDER NUMBER        IDORDNR-002         
000140     03 RIA-KDORDKL          PIC 9.                                       
000150*                                 ORDER CLASS                             
000160     03 RIA-IDARTNR          PIC 9(9).                                    
000170*                                 PART NUMBER                             
000180     03 RIA-REKSIFFR         PIC 9.                                       
000190*                                 PART NO CHECK DIGIT                     
000200     03 RIA-BERADREF         PIC X(10).                                   
000210*                                 CUSTOMERS ITEM REF.                     
000220     03 RIA-IDRONR           PIC 9(7).                                    
000230*                                 ORIGINAL ORDERNR     IDRONR-002         
000240     03 RIA-BEVOLREF         PIC X(10).                                   
000250*                                 VOLVO REFERENCE                         
000260     03 RIA-KVLEVART         PIC 9(6).                                    
000270*                                 DELIVERED QUANTITY                      
000280     03 RIA-KDRESTR          PIC 9(2).                                    
000290*                                 RESTRICTION CODE                        
000300     03 RIA-TIMM             PIC 9(2).                                    
000310*                                 MONTH (MM)                              
000320     03 RIA-TIDD             PIC 9(2).                                    
000330*                                 DAY OF MONTH (DD)                       
000340     03 RIA-TIKLOCK          PIC 9(8).                                    
000350*                                 TIME OF DAY (HHMMSSTH)                  
000360     03 FILLER               PIC X.                                       
      *** END COPY W461RIA0    LENGTH=80                                        
