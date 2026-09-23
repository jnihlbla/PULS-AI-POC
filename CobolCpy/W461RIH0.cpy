000010 01  RIH-W461RIH0.                                                        
000020*                                 ORDER. CONFIRM.    STATED QUAN-         
000030*                                 TITY IS BACK-ORDERED                    
000040*                                 TO IMPORTER                             
000050*                                 RECORD TYPE RIH                         
000060     03 RIH-IDPTYP           PIC X(3).                                    
000070*                                 RECORD TYPE                             
000080     03 RIH-KDCLAGER         PIC 9.                                       
000090*                                 CENTRAL WAREHOUSE CODE                  
000100     03 RIH-IDARTNR          PIC 9(9).                                    
000110*                                 PART NUMBER                             
000120     03 RIH-REKSIFFR         PIC 9.                                       
000130*                                 PART NO CHECK DIGIT                     
000140     03 RIH-BERADREF         PIC X(10).                                   
000150*                                 CUSTOMERS ITEM REF.                     
000160     03 RIH-IDRONR           PIC 9(7).                                    
000170*                                 ORIGINAL ORDERNR     IDRONR-002         
000180     03 RIH-BEVOLREF         PIC X(10).                                   
000190*                                 VOLVO REFERENCE                         
000200     03 RIH-KDRESTR          PIC 9(2).                                    
000210*                                 RESTRICTION CODE                        
000220     03 RIH-KVBEART          PIC 9(6).                                    
000230*                                 ORDERED QUANTITY                        
000240     03 RIH-KVAVBART         PIC 9(6).                                    
000250*                                 ALLOCATED QUANTITY                      
000260     03 RIH-KVRO             PIC 9(6).                                    
000270*                                 BACKORDERED QTY                         
000280     03 RIH-KDDSP            PIC 9.                                       
000290*                                 AFFECT ON DSP                           
000300     03 RIH-TIDISPIN         PIC 9(6).                                    
000310*                                 NEXT CONSIGNMENT AVAIL.(YYMMDD)         
000320     03 RIH-TIMM             PIC 9(2).                                    
000330*                                 MONTH (MM)                              
000340     03 RIH-TIDD             PIC 9(2).                                    
000350*                                 DAY OF MONTH (DD)                       
000360     03 RIH-TIKLOCK          PIC 9(8).                                    
000370*                                 TIME OF DAY (HHMMSSTH)                  
      *** END COPY W461RIH0    LENGTH=80                                        
