000100 01  W55142.                                                              
000200*                                  FIL MED UTDRAG UR WDK6-BASEN           
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 IDLEVNR              PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 IDANSK               PIC S9(3)           COMP-3.                  
000800*                                 ANSKAFFARNUMMER                         
000900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001000*                                 PRODUKTSLAG                             
001100     03 IDINK                PIC X(4).                                    
001200*                                 INKÖPARNUMMER                           
001300     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001400*                                 FUNKTIONSGRUPP                          
001500     03 FLIART               PIC X.                                       
001600*                                 ARTIKELN INGÅR I SATS                   
001700     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
001800*                                 INKÖPSPRIS                              
001900     03 KDVTH                PIC S9              COMP-3.                  
002000*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
002100     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
002200*                                 ARTIKELNS SJÄLVKOSTNAD                  
002300     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
002400*                                 BESTÄLLNINGSPRIS I KRONOR               
002500     03 PRDIRLON             PIC S9(4)V9(3)      COMP-3.                  
002600*                                 DIREKT LÖN                              
002700     03 PRDMTRL              PIC S9(6)V9(3)      COMP-3.                  
002800*                                 DIREKT MATERIAL                         
002900     03 PROVRPAL             PIC S9(4)V9(3)      COMP-3.                  
003000*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
003100     03 RETULF               PIC S9(3)V9(4)      COMP-3.                  
003200*                                 TULLFAKTOR                              
003300     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
003400*                                 DIREKTLEVERANSANDEL                     
003500     03 KDERS                PIC S9(3)           COMP-3.                  
003600*                                 ERSÄTTNINGSKOD                          
003700     03 KDHF                 PIC S9              COMP-3.                  
003800*                                 HUVUDFÖRRÅDSMÄRKNING                    
003900     03 KVAKS                PIC S9(7)           COMP-3.                  
004000*                                 ANKOMSTSALDO                            
004100     03 KVEFRS               PIC S9(7)           COMP-3.                  
004200*                                 EJ FAKTURERAT ANTAL STYCK               
004300     03 KVLS                 PIC S9(7)           COMP-3.                  
004400*                                 LAGERSALDO                              
004500     03 TIPRLIST             OCCURS 5 TIMES                               
004600                             PIC S9(7)           COMP-3.                  
004700*                                 PRISLISTEDATUM (AAMMDD)                 
004800     03 IDLEVNR-PR           OCCURS 5 TIMES                               
004900                             PIC X(5).                                    
005000*                                 LEVERANTÖRNR FÖR DETTA PRIS             
005100     03 PRARTBES-PR          OCCURS 5 TIMES                               
005200                             PIC S9(7)V9(2)      COMP-3.                  
005300*                                 DETTA BESTÄLLNINGSPRIS (KR)             
005400     03 PRARTBEL-PR          OCCURS 5 TIMES                               
005500                             PIC S9(8)V9(5)      COMP-3.                  
005600*                                 DETTA BESTÄLLNINGSPRIS                  
005700*                                 (I LEVERANTÖRENS VALUTA)                
005800     03 KDSTATUS-PR          OCCURS 5 TIMES                               
005900                             PIC S9              COMP-3.                  
006000*                                 STATUS PÅ DETTA PRIS                    
006100*                                 0 = PRELIMINÄR  1 = DEFINITIV           
006200     03 SUINLEV-PR           OCCURS 5 TIMES                               
006300                             PIC S9(3)           COMP-3.                  
006400*                                 ANTAL INLEV. TILL DETTA PRIS            
006500     03 KDVALISO             OCCURS 5 TIMES                               
006600                             PIC X(3).                                    
006700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006800     03 INLEV-TIPRLIST       PIC S9(7)           COMP-3.                  
006900*                                 PRISLISTEDATUM (AAMMDD)                 
007000     03 INLEV-IDLEVNR-PR     PIC X(5).                                    
007100*                                 LEVERANTÖRNR FÖR DETTA PRIS             
007200     03 INLEV-PRARTBEL-PR    PIC S9(8)V9(5)      COMP-3.                  
007300*                                 DETTA BESTÄLLNINGSPRIS                  
007400*                                 (I LEVERANTÖRENS VALUTA)                
007500     03 GODK-TIPRLIST        PIC S9(7)           COMP-3.                  
007600*                                 PRISLISTEDATUM (AAMMDD)                 
007700     03 GODK-IDLEVNR-PR      PIC X(5).                                    
007800*                                 LEVERANTÖRNR FÖR DETTA PRIS             
007900     03 GODK-PRARTBEL-PR     PIC S9(8)V9(5)      COMP-3.                  
008000*                                 DETTA BESTÄLLNINGSPRIS                  
008100*                                 (I LEVERANTÖRENS VALUTA)                
008200     03 LOCAL-TIPRLIST       PIC S9(7)           COMP-3.                  
008300*                                 PRISLISTEDATUM (AAMMDD)                 
008400     03 LOCAL-IDLEVNR-PR     PIC X(5).                                    
008500*                                 LEVERANTÖRNR FÖR DETTA PRIS             
008600     03 LOCAL-PRARTBEL-PR    PIC S9(8)V9(5)      COMP-3.                  
008700*                                 DETTA BESTÄLLNINGSPRIS                  
008800*                                 (I LEVERANTÖRENS VALUTA)                
008900*** END OF VILMAII-COPY LENGTH= 255 BYTES                                 
