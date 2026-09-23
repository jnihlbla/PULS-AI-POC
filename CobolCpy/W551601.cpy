000100 01  W551601.                                                             
000200*                                  SPIS                                   
000300*                                  ANALYS-FIL                             
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 BEART-ENG            PIC X(25).                                   
000700*                                 ENGELSK ARTIKELBENÄMNING                
000800     03 FLAPC                PIC X.                                       
000900*                                 APC FLAGGA                              
001000     03 FLIART               PIC X.                                       
001100*                                 ARTIKELN INGÅR I SATS                   
001200     03 FLPRFIL              PIC X.                                       
001300*                                 PRISHÄMTNINGSFLAGGA                     
001400     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001500*                                 FUNKTIONSGRUPP                          
001600     03 IDLEVNR              PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800     03 IDLEVNR-HUV          PIC X(5).                                    
001900*                                 LEVERANTÖRNR HUVUDLEVERANTÖR            
002000     03 IDPRANSV             PIC X(4).                                    
002100*                                 PRISANSVAR FÖR ARTIKELN                 
002200     03 KDHF                 PIC S9              COMP-3.                  
002300*                                 HUVUDFÖRRÅDSMÄRKNING                    
002400     03 KDPRBEH              PIC X.                                       
002500*                                 PRIS BEHANDLAD ARTIKEL                  
002600     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002700*                                 PRODUKTSLAG                             
002800     03 KDSTASPIS            PIC X(5).                                    
002900*                                 STATUS BESTÄLLNINGSPRIS                 
003000     03 KDVALISO             PIC X(3).                                    
003100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003200     03 KVBEHOVAR            PIC S9(7)           COMP-3.                  
003300*                                 BERÄKNAT ÅRSBEHOV AV EN ARTIKEL         
003400     03 KVDISP-SPIS          PIC S9(7)           COMP-3.                  
003500*                                 DISPONIBELT LAGER FÖR SPIS              
003600     03 PRARTBEL-PR          PIC S9(8)V9(5)      COMP-3.                  
003700*                                 DETTA BESTÄLLNINGSPRIS                  
003800*                                 (I LEVERANTÖRENS VALUTA)                
003900     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
004000*                                 BESTÄLLNINGSPRIS I KRONOR               
004100     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
004200*                                 ARTIKELNS SJÄLVKOSTNAD                  
004300     03 PRDIRLON-AKT         PIC S9(4)V9(3)      COMP-3.                  
004400*                                 DIREKT LÖN AKTUELL                      
004500     03 PRDIRLON-KOM         PIC S9(4)V9(3)      COMP-3.                  
004600*                                 DIREKT LÖN NÄSTA ÅR                     
004700     03 PRDMTRL-AKT          PIC S9(6)V9(3)      COMP-3.                  
004800*                                 DIREKT MATERIAL DETTA ÅR                
004900     03 PRDMTRL-KOM          PIC S9(6)V9(3)      COMP-3.                  
005000*                                 DIREKT MATERIAL NÄSTA ÅR                
005100     03 PRINK-AKT            PIC S9(7)V9(2)      COMP-3.                  
005200*                                 INKÖPSPRIS AKTUELLT ÅR                  
005300     03 PRINK-KOM            PIC S9(7)V9(2)      COMP-3.                  
005400*                                 INKÖPSPRIS NÄSTA ÅR                     
005500     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
005600*                                 VALUTAKURS                              
005700     03 PROVRPAL-AKT         PIC S9(4)V9(3)      COMP-3.                  
005800*                                 ÖVRIGA OMKOSTNADER PÅLÄGG,              
005900*                                 DETTA ÅR                                
006000     03 PROVRPAL-KOM         PIC S9(4)V9(3)      COMP-3.                  
006100*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
006200*                                 NÄSTA ÅR                                
006300     03 REAENDR              PIC S9(4)V9(1)      COMP-3.                  
006400*                                 ÄNDRINGSPROCENT                         
006500     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
006600*                                 DIREKTLEVERANSANDEL                     
006700     03 RETULF               PIC S9(3)V9(4)      COMP-3.                  
006800*                                 TULLFAKTOR                              
006900     03 TEARTNOT             PIC X(40).                                   
007000*                                 ARTIKEL NOTERING                        
007100     03 TIPRLIST             PIC S9(7)           COMP-3.                  
007200*                                 PRISLISTEDATUM (AAMMDD)                 
007300     03 PRARTSTD-AKT         PIC S9(7)V9(2)      COMP-3.                  
007400*                                 ARTIKELSTANDARDPRIS                     
007500     03 PRARTSTD-KOM         PIC S9(7)V9(2)      COMP-3.                  
007600*                                 ARTIKELSTANDARDPRIS                     
007700*** END OF VILMAII-COPY LENGTH= 192 BYTES                                 
