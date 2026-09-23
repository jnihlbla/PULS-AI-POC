000100 01  SEQA-WDC6A1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDC6               
000300*                                 STANDARDPRISREGISTER (KOMMANDE)         
000400*                                 FYSISK NYCKEL = WDC6A1KY                
000500*                                        (IDLEVNR + IDARTNR)              
000600*                                 SEKUNDÄR NYCKEL: WDC6ASEQ               
000700*                                                  (IDLEVNR)              
000800*                                 SÖKBEGREPP = KDPRHEH                    
000900*                                              REAENDR                    
001000     03 SEQA-IDLEVNR         PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001300     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 SEQA-KDPRBEH         PIC X.                                       
001700*                                 PRIS BEHANDLAD ARTIKEL                  
001800*                                 TREATMENT OF PART                       
001900     03 SEQA-REAENDR         PIC S9(4)V9(1)      COMP-3.                  
002000*                                 ÄNDRINGSPROCENT                         
002100*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  
