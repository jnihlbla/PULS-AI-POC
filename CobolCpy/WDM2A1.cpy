000100 01  SEQA-WDM2A1-CTX.                                                     
000200*                                 KAMPANJREGISTER                         
000300*                                 ARTIKEL INGÅNG TILL WDM211              
000400*                                 FYSISK NYCKEL: WDM2A1KY                 
000500*                                 (IDARTNR + IDKAMPRF + IDDC)             
000600*                                 SEKUNDÄR NYCKEL: WDM2ASEQ               
000700*                                 (IDARTNR)                               
000800     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 SEQA-IDKAMPRF        PIC S9(7)           COMP-3.                  
001200*                                 KAMPANJREFERENS                         
001300*                                 CAMPAIGN REFERENCE                      
001400     03 SEQA-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700*** END OF VILMAII-COPY LENGTH= 11 BYTES                                  
