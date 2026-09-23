000010 01  W4269701.                                                            
000020*                                 KVALITET                                
000030*                                 COPY-TEXT FÖR KONVERTERING              
000040     03 IDARTNR              PIC S9(9)           COMP-3.                  
000050*                                 ARTIKELNUMMER                           
000060     03 IDLEVNR              PIC S9(5)           COMP-3.                  
000070*                                 LEVERANTÖRNUMMER                        
000080     03 KDKVAKTL.                                                         
000090*                                 KVALITETSKONTROLL KOD                   
000100        05 KDKVATYP          PIC X.                                       
000110*                                 NORMAL/VERIFIKATIONS KVAL.KONTR         
000120        05 IDPROVPL-PRI      PIC X.                                       
000130*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
000140        05 IDPROVPL-SEK      PIC X.                                       
000150*                                 PROVTAGNINGSPLAN SEK.KONTROLL           
000160        05 KDKVAULG          PIC X.                                       
000170*                                 UNDERLAG FÖR KVALITETSKONTROLL          
000180     03 KVSKPLOT-PRI         PIC S9(3)           COMP-3.                  
000190*                                 SKIPLOT RÄKNARE PRIMÄR KONTROLL         
000200     03 KVSKPLOT-SEK         PIC S9(3)           COMP-3.                  
000210*                                 SKIPLOT RÄKNARE SEK. KONTROLL           
000220     03 KVSKPLOT-PRI-INIT    PIC S9(3)           COMP-3.                  
000230*                                 SKIPLOT RÄKNARE PRIMÄR KONTROLL         
000240     03 KVSKPLOT-SEK-INIT    PIC S9(3)           COMP-3.                  
000250*                                 SKIPLOT RÄKNARE SEK. KONTROLL           
000260     03 TIVV                 PIC S9(3)           COMP-3.                  
000270*                                 VECKA  (VV)                             
000280     03 KDCLAGER             PIC S9              COMP-3.                  
000290*                                 CENTRALLAGERKOD                         
000300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
000310*                                 PRODUKTSLAG                             
000320     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
000330*                                 FUNKTIONSGRUPP                          
000340     03 KVAVIS               PIC S9(7)           COMP-3.                  
000350*                                 AVISERAT ANTAL                          
000360     03 FLKVAUTV-KVAL        PIC X.                                       
000370*                                 PARTIET UTVALT KVALITETSKONTR           
      *** END COPY W4269701    LENGTH=33                                        
