000010*                                 VTAB - INTRASTAT                        
000011*                                 CENTRALT ARTIKELREGISTER                
000020*                                 BESTÅR AV 2 POSTTYPER:                  
000030*                                  11 - FILAVSÄNDARINFORMATION            
000040*                                  12 - ARTIKELINFORMATION                
000070*                                 SAMTLIGA ALFA-FÄLT SKALL VARA           
000080*                                 VÄNSTERJUSTERADE.                       
000100 01  CENTARTREG-AREA.                                                     
000300     03 POSTTYP                   PIC X(2).                               
000301*                                                                         
000310     03 FIL-AREA.                                                         
000400*                                 FILUPPGIFTER POSTTYP 11                 
000500        05 FILAVSID               PIC X(8).                               
000600        05 FILAVSNAMN             PIC X(35).                              
000700        05 AVSORGNR               PIC X(17).                              
000710*          REDOVISNINGSSKYLDIGT BOLAG                                     
000720        05 FILDATUM               PIC X(8).                               
000730        05 FILTID                 PIC X(4).                               
000770        05 FILLER                 PIC X(76).                              
000780*                                                                         
000781     03 ARTIKEL-AREA              REDEFINES FIL-AREA.                     
000782*                                 ARTIKELUPPGIFTER POSTTYP 12             
000783        05 ATGARDSKOD             PIC X(1).                               
000784*          TILLÅTNA VÄRDEN: R=NYUPPLÄGG, U=ÄNDRING,                       
000785*                           D=BORTTAG                                     
000790        05 ARTIKELNR              PIC X(14).                              
000791        05 BENAMNING-SE           PIC X(25).                              
000792        05 BENAMNING-GB           PIC X(25).                              
000793        05 LEVID                  PIC X(9).                               
000794        05 NETTOVIKT              PIC 9(7)V9(3).                          
000795        05 FUNKTIONSGRUPP         PIC X(4).                               
000796        05 ANTALSTYP              PIC X(4).                               
000797        05 ARTIKELURSPRUNG        PIC X(2).                               
000798        05 PRODUKTKOD             PIC X(2).                               
000799        05 INKNR                  PIC X(4).                               
000800        05 FORPACKNINGSKOD        PIC X(8).                               
000801        05 FARLIGTGODS            PIC X(18).                              
000802        05 STATNR                 PIC X(14).                              
000809*          ANGES ENDAST VID ÖVERENSKOMMELSE MED VTAB,                     
000810*          ANNARS BLANK                                                   
000811        05 OVRIGT                 PIC X(8).                               
000820*                                                                         
029000*** END COPY A7290B01    LENGTH=150                                       
