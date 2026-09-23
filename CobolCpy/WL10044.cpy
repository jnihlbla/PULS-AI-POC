000100 01  WL10044.                                                             
000200*                                 COPYTEXT TILL BINNING-LIST LDC          
000300*                                 CN                                      
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDARTNR              PIC Z(7)9.                                   
000700*                                 ARTIKELNUMMER                           
000800     03 BEART                PIC X(100).                                  
000900*                                 ARTIKELBENƒMNING                        
001000     03 DAREGDAT             PIC 9(8).                                    
001100*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001200     03 IDPERSON-BUY         PIC Z(2)9.                                   
001300*                                 PERSONKOD REFILLANSVARIG                
001400     03 KVAINF.                                                           
001500        05 INFO-TEKVAINF-EXT OCCURS 7 TIMES                               
001600                             PIC X(79).                                   
001700*                                 KVALITETS INFORMATION EXTERNT           
001800*** END OF VILMAII-COPY LENGTH= 607 BYTES                                 
