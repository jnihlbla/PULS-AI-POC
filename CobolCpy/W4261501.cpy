000010 01  W4261501.                                                            
000020*                                 KVALITET ERSKOD > 20                    
000030     03 IDARTNR              PIC S9(9)           COMP-3.                  
000040*                                 ARTIKELNUMMER                           
000050     03 BEART                PIC X(25).                                   
000060*                                 ARTIKELBENÄMNING                        
000070     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
000080*                                 FUNKTIONSGRUPP                          
000090     03 KDKVAKTL.                                                         
000100*                                 KVALITETSKONTROLL KOD                   
000110        05 KDKVATYP          PIC X.                                       
000120*                                 NORMAL/VERIFIKATIONS KVAL.KONTR         
000130        05 IDPROVPL-PRI      PIC X.                                       
000140*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
000150        05 IDPROVPL-SEK      PIC X.                                       
000160*                                 PROVTAGNINGSPLAN SEK.KONTROLL           
000170        05 KDKVAULG          PIC X.                                       
000180*                                 UNDERLAG FÖR KVALITETSKONTROLL          
000190     03 ADKVAULG             PIC X(2).                                    
000200*                                 PLATS UNDERLAG KVAL.KONTROLL            
000210     03 TIERSDAT             PIC S9(5)           COMP-3.                  
000220*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
000230     03 KDERS                PIC S9(3)           COMP-3.                  
000240*                                 ERSÄTTNINGSKOD                          
      *** END COPY W4261501    LENGTH=44                                        
