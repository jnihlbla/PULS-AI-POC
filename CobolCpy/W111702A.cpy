000010 01  W111702A.                                                            
000020*                                 COPYTEXT TILL FIL W11170                
000030*                                 POSTTYP 702                             
000040*                                 ERSÄTTNING (TILLK. ARTIKEL)             
000050*                                                                         
000060     03 IDPTYP               PIC X(3).                                    
000070*                                 POSTTYP                                 
000080     03 IDARTNR-ERS          PIC S9(9)           COMP-3.                  
000090*                                 ERSATT ARTIKELNUMMER                    
000100     03 IDLOPNR              PIC S9(5)           COMP-3.                  
000110*                                 LÖPNUMMER          IDLOPNR-002          
000120     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
000130*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
000140     03 IDKORTNR             PIC S9(3)           COMP-3.                  
000150*                                 KORTNUMMER                              
000160*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
000170     03 FLTEXT               PIC X.                                       
000180*                                 FINNS TEXTINFORMATION ?                 
000190     03 TYP1.                                                             
000200        05 IDARTNR-TILLK     PIC S9(9)           COMP-3.                  
000210*                                 TILLKOMMANDE ARTIKELNUMMER              
000220        05 REKSIFFR-TILLK    PIC S9              COMP-3.                  
000230*                                 KONTROLLSIFFRA                          
000240        05 DIERS-TILLK       PIC S9(4)V9(3)      COMP-3.                  
000250*                                 KVANTITET I ERSÄTTN.                    
000260        05 BEART-SVE-TILLK   PIC X(25).                                   
000270*                                 SVENSK ARTIKELBENÄMNING                 
000280        05 BEART-ENG-TILLK   PIC X(25).                                   
000290*                                 ENGELSK ARTIKELBENÄMNING                
000300        05 BEART-TYS-TILLK   PIC X(25).                                   
000310*                                 TYSK ARTIKELBENÄMNING                   
000320        05 BEART-SPA-TILLK   PIC X(25).                                   
000330*                                 SPANSK ARTIKELBENÄMNING                 
000340        05 BEART-FRA-TILLK   PIC X(25).                                   
000350*                                 FRANSK ARTIKELBENÄMNING                 
000360     03 TYP2 REDEFINES TYP1.                                              
000370        05 BEERS             PIC X(20).                                   
000380*                                 ERSÄTTNINGSTEXT                         
000390        05 FILLER            PIC X(115).                                  
000400*                                                                         
      *** END COPY W111702A    LENGTH=153                                       
