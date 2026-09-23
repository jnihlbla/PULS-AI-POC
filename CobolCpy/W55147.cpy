000100 01  W55147.                                                              
000200*                                  FIL MED UTDRAG UR WDC6-BASEN           
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 FLAPC                PIC X.                                       
000600*                                 APC FLAGGA                              
000700     03 FLIART               PIC X.                                       
000800*                                 ARTIKELN INGÅR I SATS                   
000900     03 FLPRFIL              PIC X.                                       
001000*                                 PRISHÄMTNINGSFLAGGA                     
001100     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001200*                                 FUNKTIONSGRUPP                          
001300     03 IDLEVNR              PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 IDLEVNR-HUV          PIC X(5).                                    
001600*                                 LEVERANTÖRNR HUVUDLEVERANTÖR            
001700     03 IDPRANSV             PIC X(4).                                    
001800*                                 PRISANSVAR FÖR ARTIKELN                 
001900     03 KDHF                 PIC S9              COMP-3.                  
002000*                                 HUVUDFÖRRÅDSMÄRKNING                    
002100     03 KDPRBEH              PIC X.                                       
002200*                                 PRIS BEHANDLAD ARTIKEL                  
002300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002400*                                 PRODUKTSLAG                             
002500     03 KDSTASPIS            PIC X(5).                                    
002600*                                 STATUS BESTÄLLNINGSPRIS                 
002700     03 KDVALISO             PIC X(3).                                    
002800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002900     03 KVBEHOVAR            PIC S9(7)           COMP-3.                  
003000*                                 BERÄKNAT ÅRSBEHOV AV EN ARTIKEL         
003100     03 KVDISP-SPIS          PIC S9(7)           COMP-3.                  
003200*                                 DISPONIBELT LAGER FÖR SPIS              
003300     03 PRARTBEL-PR          PIC S9(8)V9(5)      COMP-3.                  
003400*                                 DETTA BESTÄLLNINGSPRIS                  
003500*                                 (I LEVERANTÖRENS VALUTA)                
003600     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
003700*                                 BESTÄLLNINGSPRIS I KRONOR               
003800     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
003900*                                 ARTIKELNS SJÄLVKOSTNAD                  
004000     03 PRDIRLON-AKT         PIC S9(4)V9(3)      COMP-3.                  
004100*                                 DIREKT LÖN AKTUELL                      
004200     03 PRDIRLON-KOM         PIC S9(4)V9(3)      COMP-3.                  
004300*                                 DIREKT LÖN NÄSTA ÅR                     
004400     03 PRDMTRL-AKT          PIC S9(6)V9(3)      COMP-3.                  
004500*                                 DIREKT MATERIAL DETTA ÅR                
004600     03 PRDMTRL-KOM          PIC S9(6)V9(3)      COMP-3.                  
004700*                                 DIREKT MATERIAL NÄSTA ÅR                
004800     03 PRINK-AKT            PIC S9(7)V9(2)      COMP-3.                  
004900*                                 INKÖPSPRIS AKTUELLT ÅR                  
005000     03 PRINK-KOM            PIC S9(7)V9(2)      COMP-3.                  
005100*                                 INKÖPSPRIS NÄSTA ÅR                     
005200     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
005300*                                 VALUTAKURS                              
005400     03 PROVRPAL-AKT         PIC S9(4)V9(3)      COMP-3.                  
005500*                                 ÖVRIGA OMKOSTNADER PÅLÄGG,              
005600*                                 DETTA ÅR                                
005700     03 PROVRPAL-KOM         PIC S9(4)V9(3)      COMP-3.                  
005800*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
005900*                                 NÄSTA ÅR                                
006000     03 REAENDR              PIC S9(4)V9(1)      COMP-3.                  
006100*                                 ÄNDRINGSPROCENT                         
006200     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
006300*                                 DIREKTLEVERANSANDEL                     
006400     03 RETULF               PIC S9(3)V9(4)      COMP-3.                  
006500*                                 TULLFAKTOR                              
006600     03 TEARTNOT             PIC X(40).                                   
006700*                                 ARTIKEL NOTERING                        
006800     03 TIPRLIST             PIC S9(7)           COMP-3.                  
006900*                                 PRISLISTEDATUM (AAMMDD)                 
007000     03 IDNAMN               PIC X(40).                                   
007100*                                 NAMN                                    
007200     03 IDINK                PIC X(5).                                    
007300*                                 INKÖPARNUMMER                           
007400     03 IDMAIL               PIC X(60).                                   
007500*                                 MAIL ADRESS                             
007600     03 INLEV-TIPRLIST       PIC S9(7)           COMP-3.                  
007700*                                 PRISLISTEDATUM (AAMMDD)                 
007800     03 INLEV-IDLEVNR-PR     PIC X(5).                                    
007900*                                 LEVERANTÖRNR FÖR DETTA PRIS             
008000     03 INLEV-PRARTBEL-PR    PIC S9(8)V9(5)      COMP-3.                  
008100*                                 DETTA BESTÄLLNINGSPRIS                  
008200*                                 (I LEVERANTÖRENS VALUTA)                
008300     03 GODK-TIPRLIST        PIC S9(7)           COMP-3.                  
008400*                                 PRISLISTEDATUM (AAMMDD)                 
008500     03 GODK-IDLEVNR-PR      PIC X(5).                                    
008600*                                 LEVERANTÖRNR FÖR DETTA PRIS             
008700     03 GODK-PRARTBEL-PR     PIC S9(8)V9(5)      COMP-3.                  
008800*                                 DETTA BESTÄLLNINGSPRIS                  
008900*                                 (I LEVERANTÖRENS VALUTA)                
009000     03 LOCAL-TIPRLIST       PIC S9(7)           COMP-3.                  
009100*                                 PRISLISTEDATUM (AAMMDD)                 
009200     03 LOCAL-IDLEVNR-PR     PIC X(5).                                    
009300*                                 LEVERANTÖRNR FÖR DETTA PRIS             
009400     03 LOCAL-PRARTBEL-PR    PIC S9(8)V9(5)      COMP-3.                  
009500*                                 DETTA BESTÄLLNINGSPRIS                  
009600*                                 (I LEVERANTÖRENS VALUTA)                
009700*** END OF VILMAII-COPY LENGTH= 310 BYTES                                 
