000100 01  PRI-WDH801.                                                          
000200*                                 EKONOMI PRISÄNDRING                     
000300*                                 PRIS-SEGMENT                            
000400*                                 FYSISK NYCKEL: WDH801KY                 
000500*                                 (IDARTNR + DAREGDAT                     
000600*                                  + TIREGTID)                            
000700*                                 SÖKBEGREPP: TIREGDAT,                   
000800*                                  TIREGTID,IDUSER,KDPRIBEH               
000900     03 PRI-IDARTNR          PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 PRI-DAREGDAT         PIC 9(8).                                    
001300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001400*                                 REGISTRATION DATE (YYYYMMDD)            
001500     03 PRI-TIREGTID         PIC S9(7)           COMP-3.                  
001600*                                 REGISTRERINGSTID                        
001700*                                 GENERAL REGISTRATION TIME               
001800     03 PRI-FLKLAR           PIC X.                                       
001900*                                 AVSLUTNINGSMARKERING                    
002000*                                 FINISHED FLAG                           
002100     03 PRI-FLPRFIL          PIC X.                                       
002200*                                 PRISHÄMTNINGSFLAGGA                     
002300*                                 SUPPLIERS PRICE FLAG                    
002400     03 PRI-FLPRIBES         PIC X.                                       
002500*                                 SKAPA EJ BESTÄLLNINGSPRIS-INFO          
002600*                                 DO NOT UPDATE SUPPLIERS PRICE           
002700     03 PRI-FLPRIGO          PIC X.                                       
002800*                                 STOR PRISÖKNING GODKÄND                 
002900*                                 BIG PRICE INCREASE ACCETTED             
003000     03 PRI-IDUSER           PIC X(8).                                    
003100*                                 ANVÄNDARENS SÄKERHETS ID                
003200*                                 USER SECURITY-IDENTITY                  
003300     03 PRI-KDPRIBEH         PIC X.                                       
003400*                                 PRISBEHANDLINGSKOD                      
003500*                                  B = BORTTAGSMARKERAD. BEH EJ           
003600*                                  J = UPPDATERAS DIREKT                  
003700*                                  N = BEHANDLAS EJ. EJ KONTROLL.         
003800*                                  V = BEHANDLAS I VECKOKÖRNINGEN         
003900*                                 PRICE TREATMENT CODE                    
004000     03 PRI-REDIRLEV         PIC S9V9(2)         COMP-3.                  
004100*                                 DIREKTLEVERANSANDEL                     
004200     03 PRI-OLD-BEST-PRIS.                                                
004300*                                 GAMMALT BESTÄLLNINGSPRIS                
004400        05 PRI-O-IDLEVNR-PR  PIC X(5).                                    
004500*                                 LEVERANTÖRNR FÖR DETTA PRIS             
004600        05 PRI-O-KDPRURSP    PIC X.                                       
004700*                                 PRISHÄRSTAMNING BESTÄLLNING             
004800*                                 ORIGINATE ORDER PRICE                   
004900        05 PRI-O-KDSTATUS-PR PIC S9              COMP-3.                  
005000*                                 STATUS PÅ DETTA PRIS                    
005100*                                 0 = PRELIMINÄR  1 = DEFINITIV           
005200        05 PRI-O-KDVALISO    PIC X(3).                                    
005300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005400*                                 CURRENCY CODE BY ISO-STANDARD.          
005500        05 PRI-O-PRARTBEL-PR PIC S9(8)V9(5)      COMP-3.                  
005600*                                 DETTA BESTÄLLNINGSPRIS                  
005700*                                 (I LEVERANTÖRENS VALUTA)                
005800        05 PRI-O-PRARTBES-PR PIC S9(7)V9(2)      COMP-3.                  
005900*                                 DETTA BESTÄLLNINGSPRIS (KR)             
006000        05 PRI-O-SUINLEV-PR  PIC S9(3)           COMP-3.                  
006100*                                 ANTAL INLEV. TILL DETTA PRIS            
006200        05 PRI-O-TIPRLIST    PIC S9(7)           COMP-3.                  
006300*                                 PRISLISTEDATUM (AAMMDD)                 
006400     03 PRI-NEW-BEST-PRIS.                                                
006500*                                 NYTT BESTÄLLNINGSPRIS                   
006600        05 PRI-N-IDLEVNR-PR  PIC X(5).                                    
006700*                                 LEVERANTÖRNR FÖR DETTA PRIS             
006800        05 PRI-N-KDPRURSP    PIC X.                                       
006900*                                 PRISHÄRSTAMNING BESTÄLLNING             
007000*                                 ORIGINATE ORDER PRICE                   
007100        05 PRI-N-KDSTATUS-PR PIC S9              COMP-3.                  
007200*                                 STATUS PÅ DETTA PRIS                    
007300*                                 0 = PRELIMINÄR  1 = DEFINITIV           
007400        05 PRI-N-KDVALISO    PIC X(3).                                    
007500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007600*                                 CURRENCY CODE BY ISO-STANDARD.          
007700        05 PRI-N-PRARTBEL-PR PIC S9(8)V9(5)      COMP-3.                  
007800*                                 DETTA BESTÄLLNINGSPRIS                  
007900*                                 (I LEVERANTÖRENS VALUTA)                
008000        05 PRI-N-PRARTBES-PR PIC S9(7)V9(2)      COMP-3.                  
008100*                                 DETTA BESTÄLLNINGSPRIS (KR)             
008200        05 PRI-N-SUINLEV-PR  PIC S9(3)           COMP-3.                  
008300*                                 ANTAL INLEV. TILL DETTA PRIS            
008400        05 PRI-N-TIPRLIST    PIC S9(7)           COMP-3.                  
008500*                                 PRISLISTEDATUM (AAMMDD)                 
008600     03 PRI-OLD-PRIS.                                                     
008700*                                 GAMMAL PRISINFORMATION                  
008800        05 PRI-O-KDCMD       PIC X.                                       
008900*                                 RAD-UPPDATERINGSKOMMANDO                
009000*                                  BLANK  = INGENTING                     
009100*                                  D , B  = DELETE                        
009200*                                  R , Ä  = REPLACE                       
009300*                                  I , N  = INSERT                        
009400*                                  S , V  = SELECT                        
009500*                                  P , P  = PRINT                         
009600*                                 LINE UPDATE COMMAND                     
009700        05 PRI-O-PRARTBES    PIC S9(7)V9(2)      COMP-3.                  
009800*                                 BESTÄLLNINGSPRIS I KRONOR               
009900*                                 ORDER PRICE SWEDISH CURRENCY            
010000        05 PRI-O-PRARTSJK    PIC S9(7)V9(2)      COMP-3.                  
010100*                                 ARTIKELNS SJÄLVKOSTNAD                  
010200*                                 COST OF SALES                           
010300        05 PRI-O-PRARTSTD    PIC S9(7)V9(2)      COMP-3.                  
010400*                                 ARTIKELSTANDARDPRIS                     
010500*                                 STANDARD PRICE                          
010600        05 PRI-O-PRDIRLON    PIC S9(4)V9(3)      COMP-3.                  
010700*                                 DIREKT LÖN                              
010800*                                 SURCHARGE COSTS                         
010900        05 PRI-O-PRDMTRL     PIC S9(6)V9(3)      COMP-3.                  
011000*                                 DIREKT MATERIAL                         
011100*                                 SURCHARGE PACKING MATERIAL              
011200        05 PRI-O-PRINK       PIC S9(7)V9(2)      COMP-3.                  
011300*                                 INKÖPSPRIS                              
011400*                                 PURCHASE PRICE                          
011500        05 PRI-O-PRLFKST     PIC S9(3)V9(2)      COMP-3.                  
011600*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
011700*                                 SUPPLIERS PACKING AND HANDLING          
011800        05 PRI-O-PROVRPAL    PIC S9(4)V9(3)      COMP-3.                  
011900*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
012000*                                 REMAINING OVERHEAD SURCHARGE            
012100        05 PRI-O-RETULF      PIC S9(3)V9(4)      COMP-3.                  
012200*                                 TULLFAKTOR                              
012300*                                 CCY EXCH RATE INCL FREIGHT/DUTY         
012400        05 PRI-O-TEARTNOT    PIC X(40).                                   
012500*                                 ARTIKEL NOTERING                        
012600*                                 PART REMARKS NOTE                       
012700     03 PRI-NEW-PRIS.                                                     
012800*                                 NY PRISINFORMATION                      
012900        05 PRI-N-KDCMD       PIC X.                                       
013000*                                 RAD-UPPDATERINGSKOMMANDO                
013100*                                  BLANK  = INGENTING                     
013200*                                  D , B  = DELETE                        
013300*                                  R , Ä  = REPLACE                       
013400*                                  I , N  = INSERT                        
013500*                                  S , V  = SELECT                        
013600*                                  P , P  = PRINT                         
013700*                                 LINE UPDATE COMMAND                     
013800        05 PRI-N-PRARTBES    PIC S9(7)V9(2)      COMP-3.                  
013900*                                 BESTÄLLNINGSPRIS I KRONOR               
014000*                                 ORDER PRICE SWEDISH CURRENCY            
014100        05 PRI-N-PRARTSJK    PIC S9(7)V9(2)      COMP-3.                  
014200*                                 ARTIKELNS SJÄLVKOSTNAD                  
014300*                                 COST OF SALES                           
014400        05 PRI-N-PRARTSTD    PIC S9(7)V9(2)      COMP-3.                  
014500*                                 ARTIKELSTANDARDPRIS                     
014600*                                 STANDARD PRICE                          
014700        05 PRI-N-PRDIRLON    PIC S9(4)V9(3)      COMP-3.                  
014800*                                 DIREKT LÖN                              
014900*                                 SURCHARGE COSTS                         
015000        05 PRI-N-PRDMTRL     PIC S9(6)V9(3)      COMP-3.                  
015100*                                 DIREKT MATERIAL                         
015200*                                 SURCHARGE PACKING MATERIAL              
015300        05 PRI-N-PRINK       PIC S9(7)V9(2)      COMP-3.                  
015400*                                 INKÖPSPRIS                              
015500*                                 PURCHASE PRICE                          
015600        05 PRI-N-PRLFKST     PIC S9(3)V9(2)      COMP-3.                  
015700*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
015800*                                 SUPPLIERS PACKING AND HANDLING          
015900        05 PRI-N-PROVRPAL    PIC S9(4)V9(3)      COMP-3.                  
016000*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
016100*                                 REMAINING OVERHEAD SURCHARGE            
016200        05 PRI-N-RETULF      PIC S9(3)V9(4)      COMP-3.                  
016300*                                 TULLFAKTOR                              
016400*                                 CCY EXCH RATE INCL FREIGHT/DUTY         
016500        05 PRI-N-TEARTNOT    PIC X(40).                                   
016600*                                 ARTIKEL NOTERING                        
016700*                                 PART REMARKS NOTE                       
016800     03 PRI-FILLER           PIC X(10).                                   
016900*** END OF VILMAII-COPY LENGTH= 260 BYTES                                 
