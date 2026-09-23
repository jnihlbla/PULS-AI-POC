000100 01  W231227.                                                             
000200*                                 POST FÖR BERÄKNING AV                   
000300*                                 OMSÄTTNING PER LEVERANTÖR               
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDLEVNR              PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 KVPB-TOT             PIC S9(6)V9(1)      COMP-3.                  
001100*                                 TOTALT PERIODBEHOV                      
001200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001300*                                 ARTIKELSTANDARDPRIS                     
001400     03 IDPROD               PIC S9(3)           COMP-3.                  
001500*                                 PRODUKTKOD, DEL AV PRODUKTSLAG          
001600*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
