000010 01  W111701A.                                                            
000020*                                 COPYTEXT TILL FIL W11170                
000030*                                 POSTTYP 701                             
000040*                                 ERSÄTTNING (ERSATT ARTIKEL)             
000050     03 IDPTYP               PIC X(3).                                    
000060*                                 POSTTYP                                 
000070     03 IDARTNR-ERS          PIC S9(9)           COMP-3.                  
000080*                                 ERSATT ARTIKELNUMMER                    
000090     03 IDLOPNR              PIC S9(5)           COMP-3.                  
000100*                                 LÖPNUMMER          IDLOPNR-002          
000110     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
000120*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
000130     03 REKSIFFR-ERS         PIC S9              COMP-3.                  
000140*                                 KONTROLLSIFFRA                          
000150     03 KDERS-OLD            PIC S9(3)           COMP-3.                  
000160*                                 ERSÄTTNINGSKOD                          
000170     03 KDERS-NEW            PIC S9(3)           COMP-3.                  
000180*                                 ERSÄTTNINGSKOD                          
000190     03 TIERSDAT             PIC S9(5)           COMP-3.                  
000200*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
000210     03 DIERS-ERS            PIC S9(4)V9(3)      COMP-3.                  
000220*                                 ERSATT ARTIKELANTAL                     
000230     03 BEART-SVE            PIC X(25).                                   
000240*                                 SVENSK ARTIKELBENÄMNING                 
000250     03 BEART-ENG            PIC X(25).                                   
000260*                                 ENGELSK ARTIKELBENÄMNING                
000270     03 BEART-TYS            PIC X(25).                                   
000280*                                 TYSK ARTIKELBENÄMNING                   
000290     03 BEART-SPA            PIC X(25).                                   
000300*                                 SPANSK ARTIKELBENÄMNING                 
000310     03 BEART-FRA            PIC X(25).                                   
000320*                                 FRANSK ARTIKELBENÄMNING                 
      *** END COPY W111701A    LENGTH=152                                       
