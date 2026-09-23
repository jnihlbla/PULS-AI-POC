000100 01  ART-WDK101.                                                          
000200*                                 FJOLÅRETS ARTIKELINFORMATION            
000300*                                 FYSISK NYCKEL: IDARTNR                  
000400     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 ART-REKSIFFR         PIC S9              COMP-3.                  
000800*                                 KONTROLLSIFFRA                          
000900*                                 PART NO CHECK DIGIT                     
001000     03 ART-FLSPKOST         PIC X.                                       
001100*                                 SPECIELLA KOSTNADER FINS                
001200*                                 SPECIAL COSTS EXIST                     
001300     03 ART-IDANSK           PIC S9(3)           COMP-3.                  
001400*                                 ANSKAFFARNUMMER                         
001500*                                 PROCURER NO.                            
001600     03 ART-IDAVTAL          OCCURS 5 TIMES                               
001700                             PIC S9(13)          COMP-3.                  
001800*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
001900*                                 PPP   = INKÖPARNR (PREFIX)              
002000*                                 BBBBB = BESTÄLLARNR                     
002100*                                 SSS   = SUFFIX                          
002200     03 ART-IDBEST           OCCURS 5 TIMES                               
002300                             PIC S9(13)          COMP-3.                  
002400*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
002500*                                 PPP   = (PREFIX) INKÖPARNR              
002600*                                 BBBBBB= BESTÄLLARNR                     
002700*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
002800     03 ART-FILLER           PIC X(2).                                    
002900     03 ART-PRHEMTAG         PIC S9(7)V9(2)      COMP-3.                  
003000*                                 HEMTAGNINGSKOSTNAD                      
003100*                                 TRANSPORT COST                          
003200     03 ART-IDLEVNR-AVT      OCCURS 5 TIMES                               
003300                             PIC X(5).                                    
003400*                                 LEVERANTÖR ENLIGT AVTAL                 
003500     03 ART-IDLEVNR-BEST     OCCURS 5 TIMES                               
003600                             PIC X(5).                                    
003700*                                 LEVERANTÖR ENL. BESTÄLLNING             
003800     03 ART-IDLEVNR-PR       OCCURS 6 TIMES                               
003900                             PIC X(5).                                    
004000*                                 LEVERANTÖRNR FÖR DETTA PRIS             
004100     03 ART-KDAVT            PIC S9              COMP-3.                  
004200*                                 AVTALSMÄRKNING                          
004300*                                 AGREEMENT CODE                          
004400     03 ART-KDBEH-BEST       OCCURS 5 TIMES                               
004500                             PIC S9              COMP-3.                  
004600*                                 BEHANDLINGSKOD BESTÄLLNING              
004700     03 ART-KDERS-UTG        PIC S9(3)           COMP-3.                  
004800*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
004900*                                 OBSOLETION SUPERSESSION CODE            
005000     03 ART-KDPRODSL         PIC S9(3)           COMP-3.                  
005100*                                 PRODUKTSLAG                             
005200*                                 PRODUCT GROUP                           
005300     03 ART-KDSTATUS-PR      OCCURS 6 TIMES                               
005400                             PIC S9              COMP-3.                  
005500*                                 STATUS PÅ DETTA PRIS                    
005600*                                 0 = PRELIMINÄR  1 = DEFINITIV           
005700     03 ART-KDTIPPR          PIC S9              COMP-3.                  
005800*                                 TIPPAT PRIS KOD                         
005900*                                 ESTIMATED PRICE CODE                    
006000     03 ART-KVAVTANT         OCCURS 5 TIMES                               
006100                             PIC S9(7)           COMP-3.                  
006200*                                 ÅRSANTAL AVTAL                          
006300     03 ART-KVBEST-BEKR      OCCURS 5 TIMES                               
006400                             PIC S9(7)           COMP-3.                  
006500*                                 BEKRÄFTAT BESTÄLLT ANTAL                
006600     03 ART-KVBEST           OCCURS 5 TIMES                               
006700                             PIC S9(7)           COMP-3.                  
006800*                                 BESTÄLLT ANTAL                          
006900     03 ART-PRARTBEL-PR      OCCURS 6 TIMES                               
007000                             PIC S9(8)V9(5)      COMP-3.                  
007100*                                 DETTA BESTÄLLNINGSPRIS                  
007200*                                 (I LEVERANTÖRENS VALUTA)                
007300     03 ART-PRARTBES         PIC S9(7)V9(2)      COMP-3.                  
007400*                                 BESTÄLLNINGSPRIS I KRONOR               
007500*                                 ORDER PRICE SWEDISH CURRENCY            
007600     03 ART-PRARTBES-PR      OCCURS 6 TIMES                               
007700                             PIC S9(7)V9(2)      COMP-3.                  
007800*                                 DETTA BESTÄLLNINGSPRIS (KR)             
007900     03 ART-PRARTSJK         PIC S9(7)V9(2)      COMP-3.                  
008000*                                 ARTIKELNS SJÄLVKOSTNAD                  
008100*                                 COST OF SALES                           
008200     03 ART-PRARTSTD         PIC S9(7)V9(2)      COMP-3.                  
008300*                                 ARTIKELSTANDARDPRIS                     
008400*                                 STANDARD PRICE                          
008500     03 ART-PRDIRLON         PIC S9(4)V9(3)      COMP-3.                  
008600*                                 DIREKT LÖN                              
008700*                                 SURCHARGE COSTS                         
008800     03 ART-PRDMTRL          PIC S9(6)V9(3)      COMP-3.                  
008900*                                 DIREKT MATERIAL                         
009000*                                 SURCHARGE PACKING MATERIAL              
009100     03 ART-PRINK            PIC S9(7)V9(2)      COMP-3.                  
009200*                                 INKÖPSPRIS                              
009300*                                 PURCHASE PRICE                          
009400     03 ART-PROVRPAL         PIC S9(4)V9(3)      COMP-3.                  
009500*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
009600*                                 REMAINING OVERHEAD SURCHARGE            
009700     03 ART-SUINLEV-PR       OCCURS 6 TIMES                               
009800                             PIC S9(3)           COMP-3.                  
009900*                                 ANTAL INLEV. TILL DETTA PRIS            
010000     03 ART-TIAVTAL          OCCURS 5 TIMES                               
010100                             PIC S9(7)           COMP-3.                  
010200*                                 AVTALSDATUM  (ÅÅMMDD)                   
010300     03 ART-TIBEST           OCCURS 5 TIMES                               
010400                             PIC S9(7)           COMP-3.                  
010500*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
010600     03 ART-TIPRLIST         OCCURS 6 TIMES                               
010700                             PIC S9(7)           COMP-3.                  
010800*                                 PRISLISTEDATUM (AAMMDD)                 
010900     03 ART-ADLAGOMR         PIC S9(3)           COMP-3.                  
011000*                                 LAGEROMRÅDE                             
011100*                                 AREA                                    
011200     03 ART-ADGANG           PIC S9(3)           COMP-3.                  
011300*                                 GÅNG                                    
011400*                                 AISLE                                   
011500     03 ART-ADPLATS          PIC S9(5)           COMP-3.                  
011600*                                 LAGERPLATSNUMMER                        
011700*                                 LOCATION                                
011800     03 ART-IDINK            PIC X(4).                                    
011900*                                 INKÖPARNUMMER                           
012000*                                 PURCHASE IDENTIFICATION NUMBER          
012100*** END OF VILMAII-COPY LENGTH= 435 BYTES                                 
