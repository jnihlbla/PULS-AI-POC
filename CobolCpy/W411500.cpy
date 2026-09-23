000100 01  W411500-CTX.                                                         
000200*                                 TYP = 500, LÄNGD = 80                   
000300*                                                                         
000400     03 IDTYP                PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 KDCLAGER             PIC 9.                                       
001100*                                 CENTRALLAGERKOD                         
001200     03 KDFRAKT              PIC 9(2).                                    
001300*                                 FRAKTSÄTT DC TILL KUND                  
001400     03 IDORDNR              PIC 9(5).                                    
001500*                                 ORDERNUMMER UTGÅR PD90                  
001600     03 BEKUNDRF-001         PIC X(10).                                   
001700*                                 KUNDENS REFERENS                        
001800     03 TIREF1               PIC 9(6).                                    
001900*                                 KUNDREFERENS-DATUM 1   (ÅÅMMDD)         
002000     03 KDORDKL              PIC 9.                                       
002100*                                 ORDERKLASS                              
002200     03 TIPLLEVD             PIC 9(3).                                    
002300*                                 PLANERAD LEVERANSDAG (VVD)              
002400     03 TIBEGPD              PIC 9(3).                                    
002500*                                 BEGÄRD PACKNINGSDAG (VVD)               
002600     03 KDPRGRP              PIC 9(2).                                    
002700*                                 PRISGRUPP                               
002800     03 REOMRTAL             PIC 9(4).                                    
002900*                                 OMRÄKNINGSTAL                           
003000     03 KDTULLVE             PIC 9.                                       
003100*                                 TYP AV PRIS PÅ TULLFAKTURA              
003200     03 KDSPRAK              PIC X.                                       
003300*                                 SPRÅKKOD                                
003400     03 KDNOTES              PIC X(2).                                    
003500*                                 NOTERINGSKOD                            
003600     03 FLRESTN              PIC 9.                                       
003700*                                 RESTNOTERING ?                          
003800     03 KDROPACK             PIC X.                                       
003900*                                 FRISLÄPPNINGSKOD RO/DO                  
004000     03 KDFAKTYP             PIC X.                                       
004100*                                 FAKTURATYP                              
004200     03 IDPRODNR-002         PIC 9(5).                                    
004300*                                 DEL AV IDPRODNR    IDPRODNR-002         
004400     03 W411500-001-GRP.                                                  
004500        05 BEVARREF          PIC X(10).                                   
004600*                                 VÅR REFERENS                            
004700     03 W411500-002-GRP REDEFINES W411500-001-GRP.                        
004800        05 IDKONTO           PIC 9(10).                                   
004900*                                 KONTO                                   
005000     03 IDKST                PIC X(10).                                   
005100*                                 KOSTNADSSTÄLLE                          
005200     03 IDANALYS             PIC X(12).                                   
005300*                                 ANALYSNUMMER                            
005400     03 KDPERSON             PIC 9(2).                                    
005500*                                 PERSONKOD                               
005600     03 KDMASK               PIC X.                                       
005700*                                 KOD FÖR MASKINELL ORDERRAD              
005800*                                 H = SATS            W221                
005900*                                 R = RO/DO           W441                
006000*                                 S = SATS            R241                
006100*                                 D = ORDERRAD        D886                
006200*                                 B = BIPACKAD RO/DO  W411                
006300     03 IDFTG                PIC 9(2).                                    
006400*                                 FÖRETAGSID EKONOM REDOVISNING           
006500*** END OF VILMAII-COPY LENGTH= 99 BYTES                                  
