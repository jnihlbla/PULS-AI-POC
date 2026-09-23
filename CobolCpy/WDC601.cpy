000100 01  ART-WDC601.                                                          
000200*                                 STANDARDPRISREGISTER (KOMMANDE)         
000300*                                 ARTIKELINFORMATION                      
000400*                                 FYSISK NYCKEL = IDARTNR                 
000500*                                 SECKUNDÄR NYCKEL: WDC6ASEQ              
000600*                                                  (IDLEVNR)              
000700     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 ART-FLAPC            PIC X.                                       
001100*                                 APC FLAGGA                              
001200*                                 APC FLAG                                
001300     03 ART-FLIART           PIC X.                                       
001400*                                 ARTIKELN INGÅR I SATS                   
001500*                                 PART IN KIT                             
001600     03 ART-FLPRFIL          PIC X.                                       
001700*                                 PRISHÄMTNINGSFLAGGA                     
001800*                                 SUPPLIERS PRICE FLAG                    
001900     03 ART-IDFKNGRP         PIC S9(5)           COMP-3.                  
002000*                                 FUNKTIONSGRUPP                          
002100*                                 FUNCTION GROUP                          
002200     03 ART-IDLEVNR          PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002500     03 ART-IDLEVNR-HUV      PIC X(5).                                    
002600*                                 LEVERANTÖRNR HUVUDLEVERANTÖR            
002700*                                 HEAD SUPPLIER NUMBER                    
002800     03 ART-IDPRANSV         PIC X(4).                                    
002900*                                 PRISANSVAR FÖR ARTIKELN                 
003000*                                 PART PRICE RESPONSIBILITY               
003100     03 ART-KDHF             PIC S9              COMP-3.                  
003200*                                 HUVUDFÖRRÅDSMÄRKNING                    
003300*                                 CODE MAIN STORAGE                       
003400     03 ART-KDPRBEH          PIC X.                                       
003500*                                 PRIS BEHANDLAD ARTIKEL                  
003600*                                 TREATMENT OF PART                       
003700     03 ART-KDPRODSL         PIC S9(3)           COMP-3.                  
003800*                                 PRODUKTSLAG                             
003900*                                 PRODUCT GROUP                           
004000     03 ART-KDSTASPIS        PIC X(5).                                    
004100*                                 STATUS BESTÄLLNINGSPRIS                 
004200*                                 STATUS CODE ORDERED PRICE               
004300     03 ART-KDVALISO         PIC X(3).                                    
004400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004500*                                 CURRENCY CODE BY ISO-STANDARD.          
004600     03 ART-KVBEHOVAR        PIC S9(7)           COMP-3.                  
004700*                                 BERÄKNAT ÅRSBEHOV AV EN ARTIKEL         
004800*                                 ESTIMATED YEAR REQUIREMENT              
004900     03 ART-KVDISP-SPIS      PIC S9(7)           COMP-3.                  
005000*                                 DISPONIBELT LAGER FÖR SPIS              
005100*                                 QUANT AVAILABLE FOR SPIS                
005200     03 ART-PRARTBEL-PR      PIC S9(8)V9(5)      COMP-3.                  
005300*                                 DETTA BESTÄLLNINGSPRIS                  
005400*                                 (I LEVERANTÖRENS VALUTA)                
005500     03 ART-PRARTBES         PIC S9(7)V9(2)      COMP-3.                  
005600*                                 BESTÄLLNINGSPRIS I KRONOR               
005700*                                 ORDER PRICE SWEDISH CURRENCY            
005800     03 ART-PRARTSJK         PIC S9(7)V9(2)      COMP-3.                  
005900*                                 ARTIKELNS SJÄLVKOSTNAD                  
006000*                                 COST OF SALES                           
006100     03 ART-PRDIRLON-AKT     PIC S9(4)V9(3)      COMP-3.                  
006200*                                 DIREKT LÖN AKTUELL                      
006300*                                 CURRENT DIRECT WAGES                    
006400     03 ART-PRDIRLON-KOM     PIC S9(4)V9(3)      COMP-3.                  
006500*                                 DIREKT LÖN NÄSTA ÅR                     
006600*                                 DIRECT WAGES NEXT YEAR                  
006700     03 ART-PRDMTRL-AKT      PIC S9(6)V9(3)      COMP-3.                  
006800*                                 DIREKT MATERIAL DETTA ÅR                
006900*                                 SURCHARGE PACKING MATERIAL              
007000*                                 CURRENT YEAR                            
007100     03 ART-PRDMTRL-KOM      PIC S9(6)V9(3)      COMP-3.                  
007200*                                 DIREKT MATERIAL NÄSTA ÅR                
007300*                                 SURCHARGE PACKING MATERIAL              
007400*                                 NEXT YEAR                               
007500     03 ART-PRINK-AKT        PIC S9(7)V9(2)      COMP-3.                  
007600*                                 INKÖPSPRIS AKTUELLT ÅR                  
007700*                                 PURCHASE PRICE CURRENT YEAR             
007800     03 ART-PRINK-KOM        PIC S9(7)V9(2)      COMP-3.                  
007900*                                 INKÖPSPRIS NÄSTA ÅR                     
008000*                                 PURCHASE PRICE NEXT YEAR                
008100     03 ART-PRKURS           PIC S9(6)V9(5)      COMP-3.                  
008200*                                 VALUTAKURS                              
008300*                                 CURRENCY EXCHANGE RATE                  
008400     03 ART-PROVRPAL-AKT     PIC S9(4)V9(3)      COMP-3.                  
008500*                                 ÖVRIGA OMKOSTNADER PÅLÄGG,              
008600*                                 DETTA ÅR                                
008700*                                 REMAINING OVERHEAD SURCHARGE            
008800*                                 CURRENT YEAR                            
008900     03 ART-PROVRPAL-KOM     PIC S9(4)V9(3)      COMP-3.                  
009000*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
009100*                                 NÄSTA ÅR                                
009200*                                 REMAINING OVERHEAD SURCHARGE            
009300*                                 NEXT YEAR                               
009400     03 ART-REAENDR          PIC S9(4)V9(1)      COMP-3.                  
009500*                                 ÄNDRINGSPROCENT                         
009600     03 ART-REDIRLEV         PIC S9V9(2)         COMP-3.                  
009700*                                 DIREKTLEVERANSANDEL                     
009800     03 ART-RETULF           PIC S9(3)V9(4)      COMP-3.                  
009900*                                 TULLFAKTOR                              
010000*                                 CCY EXCH RATE INCL FREIGHT/DUTY         
010100     03 ART-TEARTNOT         PIC X(40).                                   
010200*                                 ARTIKEL NOTERING                        
010300*                                 PART REMARKS NOTE                       
010400     03 ART-TIPRLIST         PIC S9(7)           COMP-3.                  
010500*                                 PRISLISTEDATUM (AAMMDD)                 
010600     03 ART-FILLER           PIC X(8).                                    
010700*** END OF VILMAII-COPY LENGTH= 165 BYTES                                 
