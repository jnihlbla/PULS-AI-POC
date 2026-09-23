000010 01  ART-W6D201.                                                          
000020*                                 KVALITETSKONTROLL                       
000030*                                 ARTIKEL SEGMENT                         
000040*                                 FYSISK NYCKEL: IDARTNR                  
000050     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000060*                                 ARTIKELNUMMER                           
000070*                                 PART NUMBER                             
000080     03 ART-ADKVAULG         PIC X(2).                                    
000090*                                 PLATS UNDERLAG KVAL.KONTROLL            
000100*                                 LOCATION  QUALITY DOCUMENTATION         
000110     03 ART-KDKVAKTL.                                                     
000120*                                 KVALITETSKONTROLL KOD                   
000130*                                 QUALITY INSPECTION CODE                 
000140        05 ART-KDKVATYP      PIC X.                                       
000150*                                 NORMAL/VERIFIKATIONS KVAL.KONTR         
000160*                                 NORMAL/VERIFICATION QUAL.INSP.          
000170        05 ART-IDPROVPL-PRI  PIC X.                                       
000180*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
000190*                                 SAMPLE PLAN PRIMARY INSPECTION          
000200        05 ART-IDPROVPL-SEK  PIC X.                                       
000210*                                 PROVTAGNINGSPLAN SEK.KONTROLL           
000220*                                 SAMPLE PLAN SEC. INSPECTION             
000230        05 ART-KDKVAULG      PIC X.                                       
000240*                                 UNDERLAG FÖR KVALITETSKONTROLL          
000250*                                 DOCUMENTATION FOR QUAL.INSP.            
000260*** END COPY W6D201    LENGTH=11                                          
