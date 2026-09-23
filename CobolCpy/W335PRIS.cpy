000100 01  PRIS-W335PRIS.                                                       
000200*                                 LÄNKAREA PRISTILLÄMPNING                
000300*                                                                         
000400*                                 FYLL I PRIS-INDATAS ALLA FÄLT           
000500*                                 KDCALL=1 ANROP FÖR ATT GÖRA             
000600*                                         EN PRISTILLÄMPNING.             
000700*                                         HÄMTAR PRARTNTO                 
000800*                                                OCH/ELLER                
000900*                                                PRAVCOST OCH             
001000*                                                KDVALISO.                
001100*                                 KDCALL=2 ANROP FÖR ATT ENBART           
001200*                                          HÄMTA KDVALISO.                
001300*                                                                         
001400*                                 MÖJLIGA VÄRDEN PÅ PRIS-KDSVAR:          
001500*                                   -> NORMAL PRISTILLÄMPNING             
001600*                                 1 -> ARTIKEL SAKNAS PÅ ARTREG           
001700*                                 2 -> SEGMENT SAKNAS PÅ KUNDREG          
001800*                                 3 -> SJÄLVKOST SAKNAS PÅ WDK6           
001900*                                 VID PRIS-KDSVAR 1 ,2 ELLER 3            
002000*                                 NOLLAS ALLA PRIS-UTDATA-FÄLT            
002100*                                                                         
002200     03 PRIS-INDATA.                                                      
002300        05 PRIS-KDCALL       PIC S9(3)           COMP-3.                  
002400*                                 ANROPSTYP                               
002500        05 PRIS-IDPGM        PIC X(8).                                    
002600*                                 PROGRAM IDENTITET                       
002700        05 PRIS-IDARTNR      PIC S9(9)           COMP-3.                  
002800*                                 ARTIKELNUMMER                           
002900        05 PRIS-IDDISTR      PIC S9(5)           COMP-3.                  
003000*                                 DISTRIKTNUMMER                          
003100        05 PRIS-IDKUNDNR     PIC S9(7)           COMP-3.                  
003200*                                 KUNDNUMMER                              
003300        05 PRIS-IDDC         PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500        05 PRIS-KDORDKL      PIC S9              COMP-3.                  
003600*                                 ORDERKLASS                              
003700        05 PRIS-KVBEART      PIC S9(7)           COMP-3.                  
003800*                                 BESTÄLLT ANTAL STYCKEN                  
003900        05 PRIS-FLINVEST     PIC X.                                       
004000*                                 BYTES INVENTERINGSFLAGGA                
004100     03 PRIS-UTDATA.                                                      
004200        05 PRIS-PRARTNTO     PIC S9(7)V9(2)      COMP-3.                  
004300*                                 ARTIKELPRIS NETTO                       
004400        05 PRIS-PRARTSTD     PIC S9(7)V9(2)      COMP-3.                  
004500*                                 ARTIKELSTANDARDPRIS                     
004600        05 PRIS-PRARTSJK     PIC S9(7)V9(2)      COMP-3.                  
004700*                                 ARTIKELNS SJÄLVKOSTNAD                  
004800        05 PRIS-FLPRTILL     PIC X.                                       
004900*                                 PRISTILLÄGGS FLAGGA                     
005000        05 PRIS-KDPRTYP      PIC X.                                       
005100*                                 TYP AV PRISTILLÄMPNING                  
005200        05 PRIS-KDSVAR       PIC X.                                       
005300*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
005400        05 PRIS-PRBPRIS      PIC S9(7)V9(2)      COMP-3.                  
005500*                                 BASPRIS                                 
005600        05 PRIS-KDARTRAB     PIC 9(2).                                    
005700*                                 RABATTKOD (ARTIKELPRIS)                 
005800        05 PRIS-IDMARKBO     PIC X.                                       
005900*                                 MARKNADSBOLAGSKOD                       
006000*                                                                         
006100        05 PRIS-PRARTBTO-MARK                                             
006200                             PIC S9(7)V9(2)      COMP-3.                  
006300*                                 BRUTTOPRIS PER MARKNAD (FOB)            
006400        05 PRIS-KDVALISO     PIC X(3).                                    
006500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006600        05 PRIS-PRAVCOST     PIC S9(7)V9(2)      COMP-3.                  
006700*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
006800*** END OF VILMAII-COPY LENGTH= 69 BYTES                                  
