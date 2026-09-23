000100 01  W55150-CTX.                                                          
000200*                                 COPYTEXT FÖR FIL W55150                 
000300     03 IDLEVNR              PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
000800*                                 FUNKTIONSGRUPP                          
000900     03 IDPRANSV             PIC X(4).                                    
001000*                                 PRISANSVAR FÖR ARTIKELN                 
001100     03 IDANSK               PIC S9(3)           COMP-3.                  
001200*                                 ANSKAFFARNUMMER                         
001300     03 IDINK                PIC X(4).                                    
001400*                                 INKÖPARNUMMER                           
001500     03 KDVTH                PIC S9              COMP-3.                  
001600*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
001700     03 KDPRBEH              PIC X.                                       
001800*                                 PRIS BEHANDLAD ARTIKEL                  
001900     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
002000*                                 DIREKTLEVERANSANDEL                     
002100     03 KDHF                 PIC S9              COMP-3.                  
002200*                                 HUVUDFÖRRÅDSMÄRKNING                    
002300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002400*                                 PRODUKTSLAG                             
002500     03 FLIART               PIC X.                                       
002600*                                 ARTIKELN INGÅR I SATS                   
002700     03 FLAPC                PIC X.                                       
002800*                                 APC FLAGGA                              
002900     03 FLPRFIL              PIC X.                                       
003000*                                 PRISHÄMTNINGSFLAGGA                     
003100     03 IDLEVNR-HUV          PIC X(5).                                    
003200*                                 LEVERANTÖRNR HUVUDLEVERANTÖR            
003300     03 KVDISP-SPIS          PIC S9(7)           COMP-3.                  
003400*                                 DISPONIBELT LAGER FÖR SPIS              
003500     03 KVBEHOVAR            PIC S9(7)           COMP-3.                  
003600*                                 BERÄKNAT ÅRSBEHOV AV EN ARTIKEL         
003700     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
003800*                                 BESTÄLLNINGSPRIS I KRONOR               
003900     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
004000*                                 ARTIKELNS SJÄLVKOSTNAD                  
004100     03 PRARTBEL-PR          PIC S9(8)V9(5)      COMP-3.                  
004200*                                 DETTA BESTÄLLNINGSPRIS                  
004300*                                 (I LEVERANTÖRENS VALUTA)                
004400     03 TIPRLIST             PIC S9(7)           COMP-3.                  
004500*                                 PRISLISTEDATUM (AAMMDD)                 
004600     03 KDVALISO             PIC X(3).                                    
004700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004800     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
004900*                                 VALUTAKURS                              
005000     03 RETULF               PIC S9(3)V9(4)      COMP-3.                  
005100*                                 TULLFAKTOR                              
005200     03 TITULF               PIC S9(7)           COMP-3.                  
005300*                                 TILLÄMPNINGSDATUM FÖR                   
005400*                                 TULLFAKTOR      (ÅÅMMDD)                
005500     03 KDSTASPIS            PIC X(5).                                    
005600*                                 STATUS BESTÄLLNINGSPRIS                 
005700     03 PRINK-AKT            PIC S9(7)V9(2)      COMP-3.                  
005800*                                 INKÖPSPRIS AKTUELLT ÅR                  
005900     03 PRDIRLON-AKT         PIC S9(4)V9(3)      COMP-3.                  
006000*                                 DIREKT LÖN AKTUELL                      
006100     03 PRDMTRL-AKT          PIC S9(6)V9(3)      COMP-3.                  
006200*                                 DIREKT MATERIAL DETTA ÅR                
006300     03 PROVRPAL-AKT         PIC S9(4)V9(3)      COMP-3.                  
006400*                                 ÖVRIGA OMKOSTNADER PÅLÄGG,              
006500*                                 DETTA ÅR                                
006600     03 PRINK-KOM            PIC S9(7)V9(2)      COMP-3.                  
006700*                                 INKÖPSPRIS NÄSTA ÅR                     
006800     03 PRDIRLON-KOM         PIC S9(4)V9(3)      COMP-3.                  
006900*                                 DIREKT LÖN NÄSTA ÅR                     
007000     03 PRDMTRL-KOM          PIC S9(6)V9(3)      COMP-3.                  
007100*                                 DIREKT MATERIAL NÄSTA ÅR                
007200     03 PROVRPAL-KOM         PIC S9(4)V9(3)      COMP-3.                  
007300*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
007400*                                 NÄSTA ÅR                                
007500     03 IDNAMN               PIC X(40).                                   
007600*                                 NAMN                                    
007700     03 IDMAIL               PIC X(60).                                   
007800*                                 MAIL ADRESS                             
007900     03 INLEV-TIPRLIST       PIC S9(7)           COMP-3.                  
008000*                                 PRISLISTEDATUM (AAMMDD)                 
008100     03 INLEV-IDLEVNR-PR     PIC X(5).                                    
008200*                                 LEVERANTÖRNR FÖR DETTA PRIS             
008300     03 INLEV-PRARTBEL-PR    PIC S9(8)V9(5)      COMP-3.                  
008400*                                 DETTA BESTÄLLNINGSPRIS                  
008500*                                 (I LEVERANTÖRENS VALUTA)                
008600     03 GODK-TIPRLIST        PIC S9(7)           COMP-3.                  
008700*                                 PRISLISTEDATUM (AAMMDD)                 
008800     03 GODK-IDLEVNR-PR      PIC X(5).                                    
008900*                                 LEVERANTÖRNR FÖR DETTA PRIS             
009000     03 GODK-PRARTBEL-PR     PIC S9(8)V9(5)      COMP-3.                  
009100*                                 DETTA BESTÄLLNINGSPRIS                  
009200*                                 (I LEVERANTÖRENS VALUTA)                
009300     03 LOCAL-TIPRLIST       PIC S9(7)           COMP-3.                  
009400*                                 PRISLISTEDATUM (AAMMDD)                 
009500     03 LOCAL-IDLEVNR-PR     PIC X(5).                                    
009600*                                 LEVERANTÖRNR FÖR DETTA PRIS             
009700     03 LOCAL-PRARTBEL-PR    PIC S9(8)V9(5)      COMP-3.                  
009800*                                 DETTA BESTÄLLNINGSPRIS                  
009900*                                 (I LEVERANTÖRENS VALUTA)                
010000*** END OF VILMAII-COPY LENGTH= 273 BYTES                                 
