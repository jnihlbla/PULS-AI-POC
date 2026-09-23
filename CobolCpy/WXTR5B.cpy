000100 01  WXTR5B.                                                              
000200*                                  SPIS                                   
000300*                                  PRIMÄREXTRAKT                          
000400*                                  EXTRAKT-FIL                            
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 BEART-ENG            PIC X(25).                                   
000800*                                 ENGELSK ARTIKELBENÄMNING                
000900     03 FLAPC                PIC X.                                       
001000*                                 APC FLAGGA                              
001100     03 FLIART               PIC X.                                       
001200*                                 ARTIKELN INGÅR I SATS                   
001300     03 FLPRFIL              PIC X.                                       
001400*                                 PRISHÄMTNINGSFLAGGA                     
001500     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001600*                                 FUNKTIONSGRUPP                          
001700     03 IDLEVNR              PIC S9(5)           COMP-3.                  
001800*                                 LEVERANTÖRNUMMER                        
001900     03 IDLEVNR-HUV          PIC S9(5)           COMP-3.                  
002000*                                 LEVERANTÖRNR HUVUDLEVERANTÖR            
002100     03 IDPRANSV             PIC X(4).                                    
002200*                                 PRISANSVAR FÖR ARTIKELN                 
002300     03 KDHF                 PIC S9              COMP-3.                  
002400*                                 HUVUDFÖRRÅDSMÄRKNING                    
002500     03 KDPRBEH              PIC X.                                       
002600*                                 PRIS BEHANDLAD ARTIKEL                  
002700     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002800*                                 PRODUKTSLAG                             
002900     03 KDSTASPIS            PIC X(5).                                    
003000*                                 STATUS BESTÄLLNINGSPRIS                 
003100     03 KDVALISO             PIC X(3).                                    
003200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003300     03 KVBEHOVAR            PIC S9(7)           COMP-3.                  
003400*                                 BERÄKNAT ÅRSBEHOV AV EN ARTIKEL         
003500     03 KVDISP-SPIS          PIC S9(7)           COMP-3.                  
003600*                                 DISPONIBELT LAGER FÖR SPIS              
003700     03 PRARTBEL-PR          PIC S9(8)V9(3)      COMP-3.                  
003800*                                 DETTA BESTÄLLNINGSPRIS                  
003900*                                 (I LEVERANTÖRENS VALUTA)                
004000     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
004100*                                 BESTÄLLNINGSPRIS I KRONOR               
004200     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
004300*                                 ARTIKELNS SJÄLVKOSTNAD                  
004400     03 PRDIRLON-AKT         PIC S9(4)V9(3)      COMP-3.                  
004500*                                 DIREKT LÖN AKTUELL                      
004600     03 PRDIRLON-KOM         PIC S9(4)V9(3)      COMP-3.                  
004700*                                 DIREKT LÖN NÄSTA ÅR                     
004800     03 PRDMTRL-AKT          PIC S9(6)V9(3)      COMP-3.                  
004900*                                 DIREKT MATERIAL DETTA ÅR                
005000     03 PRDMTRL-KOM          PIC S9(6)V9(3)      COMP-3.                  
005100*                                 DIREKT MATERIAL NÄSTA ÅR                
005200     03 PRINK-AKT            PIC S9(7)V9(2)      COMP-3.                  
005300*                                 INKÖPSPRIS AKTUELLT ÅR                  
005400     03 PRINK-KOM            PIC S9(7)V9(2)      COMP-3.                  
005500*                                 INKÖPSPRIS NÄSTA ÅR                     
005600     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
005700*                                 VALUTAKURS                              
005800     03 PROVRPAL-AKT         PIC S9(4)V9(3)      COMP-3.                  
005900*                                 ÖVRIGA OMKOSTNADER PÅLÄGG,              
006000*                                 DETTA ÅR                                
006100     03 PROVRPAL-KOM         PIC S9(4)V9(3)      COMP-3.                  
006200*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
006300*                                 NÄSTA ÅR                                
006400     03 REAENDR              PIC S9(4)V9(1)      COMP-3.                  
006500*                                 ÄNDRINGSPROCENT                         
006600     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
006700*                                 DIREKTLEVERANSANDEL                     
006800     03 RETULF               PIC S9(3)V9(4)      COMP-3.                  
006900*                                 TULLFAKTOR                              
007000     03 TEARTNOT             PIC X(40).                                   
007100*                                 ARTIKEL NOTERING                        
007200     03 TIPRLIST             PIC S9(7)           COMP-3.                  
007300*                                 PRISLISTEDATUM (AAMMDD)                 
007400*                                 OBS: VID BINÄRT FORMAT LAGRAS           
007500*                                 DATUMET NEGATIVT: (-000AAMMDD)          
007600     03 PRARTSTD-AKT         PIC S9(7)V9(2)      COMP-3.                  
007700*                                 ARTIKELSTANDARDPRIS                     
007800     03 PRARTSTD-KOM         PIC S9(7)V9(2)      COMP-3.                  
007900*                                 ARTIKELSTANDARDPRIS                     
008000*** END OF VILMAII-COPY LENGTH= 187 BYTES                                 
