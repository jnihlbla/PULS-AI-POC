010000 01  W12205-CTX.                                                          
020000     03 UTFIL-TYP            PIC X.                                       
030000*                                 ALLMÄN FLAGGA                           
040000     03 FLERS                PIC X.                                       
050000*                                 TILLKOMMANDE ARTIKEL ?                  
060000     03 IDARTNR              PIC S9(9)           COMP-3.                  
070000*                                 ARTIKELNUMMER                           
080000     03 IDLEVNR              PIC X(5).                                    
090000*                                 LEVERANTÖRNUMMER                        
100000     03 IDANSK               PIC S9(3)           COMP-3.                  
110000*                                 ANSKAFFARNUMMER                         
120000     03 KVLS                 PIC S9(7)           COMP-3.                  
130000*                                 LAGERSALDO                              
140000     03 KDERS                PIC S9(3)           COMP-3.                  
150000*                                 ERSÄTTNINGSKOD                          
160000     03 KVAKS                PIC S9(7)           COMP-3.                  
170000*                                 ANKOMSTSALDO                            
180000     03 KVEFRS               PIC S9(7)           COMP-3.                  
190000*                                 EJ FAKTURERAT ANTAL STYCK               
200000     03 KVROS                PIC S9(7)           COMP-3.                  
210000*                                 RESTORDERSALDO                          
220000     03 SUTPO-TOT            PIC S9(7)           COMP-3.                  
230000*                                 TPO-KVANTITET, TOTAL                    
240000     03 KVRESS               PIC S9(7)           COMP-3.                  
250000*                                 RESERVERAT ANTAL ARTIKLAR               
260000     03 KVBR                 PIC S9(7)           COMP-3.                  
270000*                                 BESTÄLLNINGSREST                        
280000     03 KDPRODSL             PIC S9(3)           COMP-3.                  
290000*                                 PRODUKTSLAG                             
300000     03 W12205-001-GRP       OCCURS 3 TIMES.                              
310000        05 SALDO-SDC         PIC X.                                       
320000*                                 ALLMÄN FLAGGA                           
330000     03 FLDISC               PIC X.                                       
340000*                                 ALLMÄN FLAGGA                           
350000*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
