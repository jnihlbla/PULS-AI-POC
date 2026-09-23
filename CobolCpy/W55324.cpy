000100 01  W55324.                                                              
000200*                                 LISTTRANS FÖR PRISÄNDRING               
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 DAREGDAT             PIC 9(8).                                    
000600*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
000700     03 TIREGTID             PIC S9(7)           COMP-3.                  
000800*                                 REGISTRERINGSTID                        
000900     03 FLKLAR               PIC X.                                       
001000*                                 AVSLUTNINGSMARKERING                    
001100     03 FLPRFIL              PIC X.                                       
001200*                                 PRISHÄMTNINGSFLAGGA                     
001300     03 FLPRIBES             PIC X.                                       
001400*                                 SKAPA EJ BESTÄLLNINGSPRIS-INFO          
001500     03 FLPRIGO              PIC X.                                       
001600*                                 STOR PRISÖKNING GODKÄND                 
001700     03 IDUSER               PIC X(8).                                    
001800*                                 ANVÄNDARENS SÄKERHETS ID                
001900     03 KDPRIBEH             PIC X.                                       
002000*                                 PRISBEHANDLINGSKOD                      
002100*                                  B = BORTTAGSMARKERAD. BEH EJ           
002200*                                  J = UPPDATERAS DIREKT                  
002300*                                  N = BEHANDLAS EJ. EJ KONTROLL.         
002400*                                  V = BEHANDLAS I VECKOKÖRNINGEN         
002500     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
002600*                                 DIREKTLEVERANSANDEL                     
002700     03 OLD-BEST-PRIS.                                                    
002800*                                 GAMMALT BESTÄLLNINGSPRIS                
002900        05 O-IDLEVNR-PR      PIC X(5).                                    
003000*                                 LEVERANTÖRNR FÖR DETTA PRIS             
003100        05 O-KDPRURSP        PIC X.                                       
003200*                                 PRISHÄRSTAMNING BESTÄLLNING             
003300        05 O-KDSTATUS-PR     PIC S9              COMP-3.                  
003400*                                 STATUS PÅ DETTA PRIS                    
003500*                                 0 = PRELIMINÄR  1 = DEFINITIV           
003600        05 O-KDVALISO        PIC X(3).                                    
003700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003800        05 O-PRARTBEL-PR     PIC S9(8)V9(5)      COMP-3.                  
003900*                                 DETTA BESTÄLLNINGSPRIS                  
004000*                                 (I LEVERANTÖRENS VALUTA)                
004100        05 O-PRARTBES-PR     PIC S9(7)V9(2)      COMP-3.                  
004200*                                 DETTA BESTÄLLNINGSPRIS (KR)             
004300        05 O-SUINLEV-PR      PIC S9(3)           COMP-3.                  
004400*                                 ANTAL INLEV. TILL DETTA PRIS            
004500        05 O-TIPRLIST        PIC S9(7)           COMP-3.                  
004600*                                 PRISLISTEDATUM (AAMMDD)                 
004700     03 NEW-BEST-PRIS.                                                    
004800*                                 NYTT BESTÄLLNINGSPRIS                   
004900        05 N-IDLEVNR-PR      PIC X(5).                                    
005000*                                 LEVERANTÖRNR FÖR DETTA PRIS             
005100        05 N-KDPRURSP        PIC X.                                       
005200*                                 PRISHÄRSTAMNING BESTÄLLNING             
005300        05 N-KDSTATUS-PR     PIC S9              COMP-3.                  
005400*                                 STATUS PÅ DETTA PRIS                    
005500*                                 0 = PRELIMINÄR  1 = DEFINITIV           
005600        05 N-KDVALISO        PIC X(3).                                    
005700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005800        05 N-PRARTBEL-PR     PIC S9(8)V9(5)      COMP-3.                  
005900*                                 DETTA BESTÄLLNINGSPRIS                  
006000*                                 (I LEVERANTÖRENS VALUTA)                
006100        05 N-PRARTBES-PR     PIC S9(7)V9(2)      COMP-3.                  
006200*                                 DETTA BESTÄLLNINGSPRIS (KR)             
006300        05 N-SUINLEV-PR      PIC S9(3)           COMP-3.                  
006400*                                 ANTAL INLEV. TILL DETTA PRIS            
006500        05 N-TIPRLIST        PIC S9(7)           COMP-3.                  
006600*                                 PRISLISTEDATUM (AAMMDD)                 
006700     03 OLD-PRIS.                                                         
006800*                                 GAMMAL PRISINFORMATION                  
006900        05 O-KDCMD           PIC X.                                       
007000*                                 RAD-UPPDATERINGSKOMMANDO                
007100*                                  BLANK  = INGENTING                     
007200*                                  D , B  = DELETE                        
007300*                                  R , Ä  = REPLACE                       
007400*                                  I , N  = INSERT                        
007500*                                  S , V  = SELECT                        
007600*                                  P , P  = PRINT                         
007700        05 O-PRARTBES        PIC S9(7)V9(2)      COMP-3.                  
007800*                                 BESTÄLLNINGSPRIS I KRONOR               
007900        05 O-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
008000*                                 ARTIKELNS SJÄLVKOSTNAD                  
008100        05 O-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
008200*                                 ARTIKELSTANDARDPRIS                     
008300        05 O-PRDIRLON        PIC S9(4)V9(3)      COMP-3.                  
008400*                                 DIREKT LÖN                              
008500        05 O-PRDMTRL         PIC S9(6)V9(3)      COMP-3.                  
008600*                                 DIREKT MATERIAL                         
008700        05 O-PRINK           PIC S9(7)V9(2)      COMP-3.                  
008800*                                 INKÖPSPRIS                              
008900        05 O-PRLFKST         PIC S9(3)V9(2)      COMP-3.                  
009000*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
009100        05 O-PROVRPAL        PIC S9(4)V9(3)      COMP-3.                  
009200*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
009300        05 O-RETULF          PIC S9(3)V9(4)      COMP-3.                  
009400*                                 TULLFAKTOR                              
009500        05 O-TEARTNOT        PIC X(40).                                   
009600*                                 ARTIKEL NOTERING                        
009700     03 NEW-PRIS.                                                         
009800*                                 NY PRISINFORMATION                      
009900        05 N-KDCMD           PIC X.                                       
010000*                                 RAD-UPPDATERINGSKOMMANDO                
010100*                                  BLANK  = INGENTING                     
010200*                                  D , B  = DELETE                        
010300*                                  R , Ä  = REPLACE                       
010400*                                  I , N  = INSERT                        
010500*                                  S , V  = SELECT                        
010600*                                  P , P  = PRINT                         
010700        05 N-PRARTBES        PIC S9(7)V9(2)      COMP-3.                  
010800*                                 BESTÄLLNINGSPRIS I KRONOR               
010900        05 N-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
011000*                                 ARTIKELNS SJÄLVKOSTNAD                  
011100        05 N-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
011200*                                 ARTIKELSTANDARDPRIS                     
011300        05 N-PRDIRLON        PIC S9(4)V9(3)      COMP-3.                  
011400*                                 DIREKT LÖN                              
011500        05 N-PRDMTRL         PIC S9(6)V9(3)      COMP-3.                  
011600*                                 DIREKT MATERIAL                         
011700        05 N-PRINK           PIC S9(7)V9(2)      COMP-3.                  
011800*                                 INKÖPSPRIS                              
011900        05 N-PRLFKST         PIC S9(3)V9(2)      COMP-3.                  
012000*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
012100        05 N-PROVRPAL        PIC S9(4)V9(3)      COMP-3.                  
012200*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
012300        05 N-RETULF          PIC S9(3)V9(4)      COMP-3.                  
012400*                                 TULLFAKTOR                              
012500        05 N-TEARTNOT        PIC X(40).                                   
012600*                                 ARTIKEL NOTERING                        
012700*** END OF VILMAII-COPY LENGTH= 250 BYTES                                 
