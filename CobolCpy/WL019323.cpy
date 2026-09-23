000100 01  B23-WL019323.                                                        
000200*                                 COPYTEXT FOR BOLLA PARZIALE OCH         
000300*                                  TOTALE CUST-TOTAL-LINE                 
000400     03 B23-IDAFPRCD         PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 B23-KVKOLLI-SUM-KUND PIC Z(3)9.                                   
000700*                                 ANTAL KOLLI                             
000800     03 B23-VKORDBTO-SUM-KUND                                             
000900                             PIC Z(5)9.9.                                 
001000*                                 ORDERVIKT BRUTTO (KG)                   
001100     03 B23-VLORDBTO-SUM-KUND                                             
001200                             PIC Z(3)9.9(3).                              
001300*                                 ORDERVOLYM BRUTTO (M3)                  
001400*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
