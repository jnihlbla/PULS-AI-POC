000010*** EDIT ALLOWED                                                          
000100 01  W475030-CTX.                                                         
000200*                                 POSTTYP 03  FÖR TULLREST.               
000300*                                                                         
000400     03 TREST03-IDPTYP2      PIC X(2).                                    
000500*                                 POSTTYP                                 
000600     03 TREST03-IDARTNR      PIC X(14).                                   
000700*                                 ARTIKELNUMMER                           
000800     03 TREST03-IDDISTR      PIC 9(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 TREST03-IDFAKT       PIC X(12).                                   
001100*                                 FAKTURANUMMER                           
001200     03 TREST03-DAFAKT       PIC 9(8).                                    
001300*                                 FAKTURADATUM  (CCYYMMDD)                
001400     03 TREST03-KOD          PIC X.                                       
001500*                                 FAST VÄRDE 'N'                          
001501     03 TREST03-KVLEVART     PIC 9(7).                                    
001502*                                 LEVERERAT ANTAL                         
001503     03 TREST03-IDLANDX2     PIC X(2).                                    
001504*                                 LANDKOD MOTTAGANDE LAND                 
001510     03 FILLER               PIC X(30).                                   
001520*                                 FILEN SKALL VARA 80 LÅNG                
001600*** END COPY W475010    LENGTH= 80 BYTES                                  
