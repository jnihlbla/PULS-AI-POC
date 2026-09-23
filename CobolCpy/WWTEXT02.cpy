000100*** EDIT ALLOWED                                                          
060007*                            *************************************        
060008*                            *** USED IN TESTING OF:                      
060009*                            ***  - CUSTOMER CATEGORY CODE                
060010*                            ***    TO SEARCH FOR CUSTOMER                
060020*                            ***    CATEGORY CODE DESCRIPTION.            
060030*                            ***                                          
060040*                            ***    USE SEARCH ALL TO SEARCH IN           
060050*                            ***    THE TABLE.                            
060060*                            *************************************        
060070*                                                                         
060080*   TABLE TO SEARCH CUSTOMER CATEGORY.                                    
060090*                                                                         
060100 01  TEXT02-KDKUNDKAT                 PIC 9(2).                           
060200                                                                          
060300     88  GOOD-KDKUNDKAT               VALUE 01 THRU 18.                   
060400                                                                          
060500     88  KDKUNDKAT-DEALER             VALUE 01.                           
060600     88  KDKUNDKAT-POLESTAR           VALUE 02.                           
060700     88  KDKUNDKAT-LYNC-N-CO          VALUE 03.                           
060701     88  KDKUNDKAT-IMPORTER           VALUE 04.                           
060702     88  KDKUNDKAT-INTERNAL-CUSTOMER  VALUE 05.                           
060703     88  KDKUNDKAT-SALES-COMPANY      VALUE 06.                           
060704     88  KDKUNDKAT-SUPPLIER-ORDERS    VALUE 07.                           
060705     88  KDKUNDKAT-REFILL             VALUE 08.                           
060706     88  KDKUNDKAT-TRANSFER           VALUE 09.                           
060707     88  KDKUNDKAT-EXTENDED-REFILL    VALUE 10.                           
060708     88  KDKUNDKAT-RETURNS            VALUE 11.                           
060709     88  KDKUNDKAT-QUALITY-RETURNS    VALUE 12.                           
060710     88  KDKUNDKAT-SCRAP              VALUE 13.                           
060711     88  KDKUNDKAT-QUALITY-SCRAP      VALUE 14.                           
060712     88  KDKUNDKAT-MIXED-STOCK        VALUE 15.                           
060713     88  KDKUNDKAT-INT-EXCHANGE-ORDER VALUE 16.                           
060714     88  KDKUNDKAT-EMBALLAGE          VALUE 17.                           
060715     88  KDKUNDKAT-ECOM               VALUE 18.                           
060800                                                                          
060900 01  TEXT02-KDKUNDKAT-DESC            PIC X(10).                          
061000                                                                          
061100     88  GOOD-KDKUNDKAT-DESC          VALUE 'DEALER','POLESTAR'           
061200                                          ,'LYNC&CO','IMPORTER'           
061201                                          ,'INTCUST','SALESCMPNY'         
061202                                          ,'SUPPLRORDR','REFILL'          
061203                                          ,'TRANSFER'                     
061204                                          ,'EXTNDREFIL'                   
061205                                          ,'RETURNS','QLTYRETRNS'         
061206                                          ,'SCRAP','QLTYSCRAP'            
061207                                          ,'MXDSTOCK'                     
061208                                          ,'INTEXCHGOR'                   
061209                                          ,'EMBALLAGE'                    
061210                                          ,'ECOM'.                        
061211                                                                          
061212     88  KDKUNDKAT-DEALER             VALUE 'DEALER'.                     
061213     88  KDKUNDKAT-POLESTAR           VALUE 'POLESTAR'.                   
061214     88  KDKUNDKAT-LYNC-N-CO          VALUE 'LYNC&CO'.                    
061215     88  KDKUNDKAT-IMPORTER           VALUE 'IMPORTER'.                   
061216     88  KDKUNDKAT-INTERNAL-CUSTOMER  VALUE 'INTCUST'.                    
061217     88  KDKUNDKAT-SALES-COMPANY      VALUE 'SALESCMPNY'.                 
061218     88  KDKUNDKAT-SUPPLIER-ORDERS    VALUE 'SUPPLRORDR'.                 
061219     88  KDKUNDKAT-REFILL             VALUE 'REFILL'.                     
061220     88  KDKUNDKAT-TRANSFER           VALUE 'TRANSFER'.                   
061221     88  KDKUNDKAT-EXTENDED-REFILL    VALUE 'EXTNDREFIL'.                 
061222     88  KDKUNDKAT-RETURNS            VALUE 'RETURNS'.                    
061223     88  KDKUNDKAT-QUALITY-RETURNS    VALUE 'QLTYRETRNS'.                 
061224     88  KDKUNDKAT-SCRAP              VALUE 'SCRAP'.                      
061225     88  KDKUNDKAT-QUALITY-SCRAP      VALUE 'QLTYSCRAP'.                  
061226     88  KDKUNDKAT-MIXED-STOCK        VALUE 'MXDSTOCK'.                   
061227     88  KDKUNDKAT-INT-EXCHANGE-ORDER VALUE 'INTEXCHGOR'.                 
061228     88  KDKUNDKAT-EMBALLAGE          VALUE 'EMBALLAGE'.                  
061229     88  KDKUNDKAT-ECOM               VALUE 'ECOM'.                       
061600                                                                          
061700 01  TEXT02-TABLE-VALUES.                                                 
061800     03  FILLER             PIC X(12) VALUE '01DEALER    '.               
061900     03  FILLER             PIC X(12) VALUE '02POLESTAR  '.               
062000     03  FILLER             PIC X(12) VALUE '03LYNC&CO   '.               
062001     03  FILLER             PIC X(12) VALUE '04IMPORTER  '.               
062002     03  FILLER             PIC X(12) VALUE '05INTCUST   '.               
062003     03  FILLER             PIC X(12) VALUE '06SALESCMPNY'.               
062004     03  FILLER             PIC X(12) VALUE '07SUPPLRORDR'.               
062005     03  FILLER             PIC X(12) VALUE '08REFILL    '.               
062006     03  FILLER             PIC X(12) VALUE '09TRANSFER  '.               
062007     03  FILLER             PIC X(12) VALUE '10EXTNDREFIL'.               
062008     03  FILLER             PIC X(12) VALUE '11RETURNS   '.               
062009     03  FILLER             PIC X(12) VALUE '12QLTYRETRNS'.               
062010     03  FILLER             PIC X(12) VALUE '13SCRAP     '.               
062011     03  FILLER             PIC X(12) VALUE '14QLTYSCRAP '.               
062012     03  FILLER             PIC X(12) VALUE '15MXDSTOCK  '.               
062013     03  FILLER             PIC X(12) VALUE '16INTEXCHGOR'.               
062014     03  FILLER             PIC X(12) VALUE '17EMBALLAGE '.               
062015     03  FILLER             PIC X(12) VALUE '18ECOM      '.               
062100*                                                                         
062200 01  TEXT02-TAB REDEFINES TEXT02-TABLE-VALUES.                            
062300     03  TEXT02-KDKUNDKAT-TAB OCCURS 18 TIMES                             
062400                      ASCENDING KEY IS TEXT02-KDKUNDKAT-REC               
062500                      INDEXED BY TEXT02-IX.                               
062600       05  TEXT02-KDKUNDKAT-REC.                                          
062700           07 TEXT02-KDKUNDKAT     PIC 9(2).                              
062800       05  TEXT02-BEKUNDKAT        PIC X(10).                             
