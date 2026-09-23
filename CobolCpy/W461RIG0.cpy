000010 01  RIG-W461RIG0.                                                        
000020*                                 ORDER. CONFIRM.  QUANTITY ADAP-         
000030*                                 TION  TO IMPORTER                       
000040*                                 RECORD TYPE RIG                         
000050     03 RIG-IDPTYP           PIC X(3).                                    
000060*                                 RECORD TYPE                             
000070     03 RIG-KDCLAGER         PIC 9.                                       
000080*                                 CENTRAL WAREHOUSE CODE                  
000090     03 RIG-IDARTNR          PIC 9(9).                                    
000100*                                 PART NUMBER                             
000110     03 RIG-REKSIFFR         PIC 9.                                       
000120*                                 PART NO CHECK DIGIT                     
000130     03 RIG-BERADREF         PIC X(10).                                   
000140*                                 CUSTOMERS ITEM REF.                     
000150     03 RIG-IDRONR           PIC 9(7).                                    
000160*                                 ORIGINAL ORDERNR     IDRONR-002         
000170     03 RIG-BEVOLREF         PIC X(10).                                   
000180*                                 VOLVO REFERENCE                         
000190     03 RIG-KDRESTR          PIC 9(2).                                    
000200*                                 RESTRICTION CODE                        
000210     03 RIG-KVBEART          PIC 9(6).                                    
000220*                                 ORDERED QUANTITY                        
000230     03 RIG-KVBEART-Q        PIC 9(6).                                    
000240*                                 ORDERED QUANTITY ADAPTED                
000250*                                  ITEMS                                  
000260     03 RIG-KVQPACK-1        PIC 9(5).                                    
000270*                                 QUANTITY IN BULK PACK Q1                
000280     03 RIG-KDDSP            PIC 9.                                       
000290*                                 AFFECT ON DSP                           
000300     03 RIG-TIMM             PIC 9(2).                                    
000310*                                 MONTH (MM)                              
000320     03 RIG-TIDD             PIC 9(2).                                    
000330*                                 DAY OF MONTH (DD)                       
000340     03 RIG-TIKLOCK          PIC 9(8).                                    
000350*                                 TIME OF DAY (HHMMSSTH)                  
000360     03 FILLER               PIC X(7).                                    
      *** END COPY W461RIG0    LENGTH=80                                        
