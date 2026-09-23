000100 01  SKOLLI-WDE121.                                                       
000200*                                 TRANSPORTRELEASEREGISTER                
000300*                                 KOLLIINFO                               
000400*                                 FYSISK NYCKEL: WDE121KY                 
000500*                                 (IDPRODNR, IDKOLLI)                     
000600     03 SKOLLI-IDPRODNR      PIC S9(7)           COMP-3.                  
000700*                                 PRODUKTIONSNUMMER                       
000800*                                 PRODUCTION NUMBER                       
000900     03 SKOLLI-IDKOLLI       PIC S9(5)           COMP-3.                  
001000*                                 KOLLINUMMER                             
001100*                                 CASE NUMBER                             
001200     03 SKOLLI-DIKOLLIB      PIC S9(3)           COMP-3.                  
001300*                                 KOLLI-BREDD                             
001400*                                 CASE WIDTH                              
001500     03 SKOLLI-DIKOLLIH      PIC S9(3)           COMP-3.                  
001600*                                 KOLLI-HÖJD                              
001700*                                 CASE HEIGHT                             
001800     03 SKOLLI-DIKOLLIL      PIC S9(5)           COMP-3.                  
001900*                                 KOLLI-LÄNGD                             
002000*                                 CASE LENGTH                             
002100     03 SKOLLI-FLDIRLEV      PIC X.                                       
002200*                                 DIREKTLEVERANS ?                        
002300*                                 DIRECT DELIVERY ?                       
002400     03 SKOLLI-IDORDER       PIC S9(7)           COMP-3.                  
002500*                                 VOLVO PARTS ORDERNUMMER                 
002600*                                 VOLVO PARTS ORDER NUMBER                
002700     03 SKOLLI-KDEMBTYP      PIC S9              COMP-3.                  
002800*                                 EMBALLAGETYP                            
002900*                                 PACKAGE TYPE                            
003000     03 SKOLLI-KDFARLIG-KOLLI                                             
003100                             PIC S9              COMP-3.                  
003200*                                 KOD FÖR FARLIGT GODS I KOLLI            
003300*                                 CODE FOR DANG GOODS IN A CASE           
003400     03 SKOLLI-KDFRAKT       PIC S9(3)           COMP-3.                  
003500*                                 FRAKTSÄTT DC TILL KUND                  
003600*                                 FREIGHT CODE                            
003700     03 SKOLLI-KDKOLLI       PIC X(8).                                    
003800*                                 KOLLIKOD                                
003900*                                 KOLLI CODE                              
004000     03 SKOLLI-KVFLAMP-KOLLI PIC S9(2)V9(1)      COMP-3.                  
004100*                                 KOLLITS FLAMPUNKT                       
004200*                                 FLASH POINT FOR A CASE                  
004300     03 SKOLLI-DEAL-PR-SUM.                                               
004400*                                 DEALERPRIS (HUVUD)                      
004500        05 SKOLLI-SUORDV-LOC PIC S9(9)V9(2)      COMP-3.                  
004600*                                 ORDERVÄRDE SLUTKUNDPRIS                 
004700*                                 I LOKAL VALUTA                          
004800*                                 ORDER VALUE, CUSTOMER PRICE             
004900*                                 IN LOCAL CURRENCY                       
005000        05 SKOLLI-SUORDV-LOCPREL                                          
005100                             PIC S9(9)V9(2)      COMP-3.                  
005200*                                 ORDERVÄRDE PREL SLUT-                   
005300*                                 KUNDPRIS, LOKAL VALUTA                  
005400*                                 ORDER VALUE, PREL CUSTOMER              
005500*                                 PRICE IN LOCAL CURRENCY                 
005600        05 SKOLLI-KDVALISO   PIC X(3).                                    
005700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005800*                                 CURRENCY CODE BY ISO-STANDARD.          
005900     03 SKOLLI-SUORDV        PIC S9(9)V9(2)      COMP-3.                  
006000*                                 SUMMA ORDERVÄRDE                        
006100*                                 TOTAL ORDER VALUE                       
006200     03 SKOLLI-TIPACKN       PIC S9(7)           COMP-3.                  
006300*                                 PACKNINGSDATUM         (ÅÅMMDD)         
006400*                                 PACKING DATE           (YYMMDD)         
006500     03 SKOLLI-VKORDBTO-KOLLI                                             
006600                             PIC S9(6)V9(1)      COMP-3.                  
006700*                                 ORDERVIKT BRUTTO PER KOLLI              
006800*                                 ORDER WEIGHT GROSS PER CASE             
006900     03 SKOLLI-VKORDNTO-KOLLI                                             
007000                             PIC S9(6)V9(1)      COMP-3.                  
007100*                                 ORDERVIKT NETTO PER KOLLI               
007200*                                 ORDER WEIGHT NET PER CASE               
007300     03 SKOLLI-VLORDBTO-KOLLI                                             
007400                             PIC S9(4)V9(3)      COMP-3.                  
007500*                                 ORDERVOLYM BRUTTO KOLLI                 
007600*                                 ORDER VOL GR/CASE                       
007700     03 SKOLLI-IDGMTREF.                                                  
007800*                                 GODSMOTTAGAREREFERENS                   
007900*                                 GOODS RECEIVER REFERENS                 
008000        05 SKOLLI-IDDISTR    PIC S9(5)           COMP-3.                  
008100*                                 DISTRIKTNUMMER                          
008200*                                 DISTRICT NUMBER                         
008300        05 SKOLLI-IDKUNDNR   PIC S9(7)           COMP-3.                  
008400*                                 KUNDNUMMER                              
008500*                                 CUSTOMER NO                             
008600        05 SKOLLI-IDKUNDRF-GRP.                                           
008700*                                 KUNDENS REFERENS (ORDERID)              
008800*                                 CUSTOMER REFERENCE (ORDER ID)           
008900           07 SKOLLI-IDKUNDRF                                             
009000                             PIC X(10).                                   
009100*                                 KUNDENS REFERENS (ORDERID)              
009200*                                 CUSTOMER REFERENCE (ORDER ID)           
009300           07 SKOLLI-IDORDNR5-FILLER REDEFINES SKOLLI-IDKUNDRF.           
009400              09 SKOLLI-IDORDNR5                                          
009500                             PIC 9(5).                                    
009600*                                 ORDERNUMMER                             
009700*                                 ORDER NUMBER                            
009800              09 FILLER      PIC X(5).                                    
009900           07 SKOLLI-IDORDNR7-FILLER REDEFINES SKOLLI-IDKUNDRF.           
010000              09 SKOLLI-IDORDNR7                                          
010100                             PIC 9(7).                                    
010200*                                 ORDERNUMMER                             
010300*                                 ORDER NUMBER                            
010400              09 FILLER      PIC X(3).                                    
010500     03 SKOLLI-KDFAKTYP      PIC X.                                       
010600*                                 FAKTURATYP                              
010700*                                 INVOICE TYPE                            
010800     03 SKOLLI-KDORDKL       PIC S9              COMP-3.                  
010900*                                 ORDERKLASS                              
011000*                                 ORDER CLASS                             
011100     03 SKOLLI-TIORDREG      PIC S9(7)           COMP-3.                  
011200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
011300*                                 ORDER REGISTRATION DATE  YYMMDD         
011400     03 SKOLLI-IDFAKT        PIC S9(7)           COMP-3.                  
011500*                                 FAKTURANUMMER                           
011600*                                 INVOICE NO.                             
011700     03 SKOLLI-IDKOLLI-SAMP  PIC S9(5)           COMP-3.                  
011800*                                 SAMPACKNINGSKOLLINUMMER                 
011900*                                 MIXED PACKING CASE NUMBER               
012000     03 SKOLLI-SUORDV-EXP    PIC S9(9)V9(2)      COMP-3.                  
012100*                                 SUMMA ORDERVÄRDE EXPORTFLÖDE            
012200*                                 TOTAL ORDER VALUE EXPORT FLOW           
012300     03 SKOLLI-IDFAKT-EXP    PIC S9(7)           COMP-3.                  
012400*                                 FAKTNR NR.1 I EXPORTFLÖDET              
012500*                                 INVOICE NO 1 IN EXPORT FLOW             
012600     03 SKOLLI-KDVALISO-EXP  PIC X(3).                                    
012700*                                 VALUTAKOD I EXP.FLÖDE(LOK. VAL)         
012800*                                 CURRENCY FOR EXPORT (LOC. CURR)         
012900     03 SKOLLI-FLCROSS       PIC X.                                       
013000*                                 CROSS-DOCKING FLAGGA                    
013100*                                 CROSS DOCKING FLAG                      
013200*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
