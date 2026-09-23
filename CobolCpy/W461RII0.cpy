000010 01  RII-W461RII0.                                                        
000020*                                 ORDER. CONFIRM. CANCELLED LINES         
000030*                                  TO IMPORTER    RECORD TYPE RII         
000040     03 RII-IDPTYP           PIC X(3).                                    
000050*                                 RECORD TYPE                             
000060     03 RII-KDCLAGER         PIC 9.                                       
000070*                                 CENTRAL WAREHOUSE CODE                  
000080     03 RII-IDARTNR          PIC 9(9).                                    
000090*                                 PART NUMBER                             
000100     03 RII-REKSIFFR         PIC 9.                                       
000110*                                 PART NO CHECK DIGIT                     
000120     03 RII-BERADREF         PIC X(10).                                   
000130*                                 CUSTOMERS ITEM REF.                     
000140     03 RII-IDRONR           PIC 9(7).                                    
000150*                                 ORIGINAL ORDERNR     IDRONR-002         
000160     03 RII-BEVOLREF         PIC X(10).                                   
000170*                                 VOLVO REFERENCE                         
000180     03 RII-KDRESTR          PIC 9(2).                                    
000190*                                 RESTRICTION CODE                        
000200     03 RII-KVBEART          PIC 9(6).                                    
000210*                                 ORDERED QUANTITY                        
000220     03 RII-KDDSP            PIC 9.                                       
000230*                                 AFFECT ON DSP                           
000240     03 RII-TIMM             PIC 9(2).                                    
000250*                                 MONTH (MM)                              
000260     03 RII-TIDD             PIC 9(2).                                    
000270*                                 DAY OF MONTH (DD)                       
000280     03 RII-TIKLOCK          PIC 9(8).                                    
000290*                                 TIME OF DAY (HHMMSSTH)                  
000300     03 FILLER               PIC X(18).                                   
      *** END COPY W461RII0    LENGTH=80                                        
