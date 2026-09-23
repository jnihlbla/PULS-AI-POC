000100 01  W4O56201.                                                            
000200*                                 COPYTEXT FÖR MOD W4O56201               
000300*                                                                         
000400     03 TRANS-NUMMER         PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MESSAGE-RAD1         PIC X(40).                                   
000700*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000800     03 IDLASTNR-IN          PIC X(7).                                    
000900*                                 LASTNINGSNUMMER                         
001000     03 KDTRSTAT-IN          PIC X.                                       
001100*                                 TRANSAKTIONSSTATUS                      
001200     03 IDLASTNR-UT          PIC X(7).                                    
001300*                                 LASTNINGSNUMMER                         
001400     03 KDTRSTAT-UT          PIC X.                                       
001500*                                 TRANSAKTIONSSTATUS                      
001600     03 KDBILD               PIC X.                                       
001700*                                 BILDSTATUS                              
001800     03 IDPRODNR-NEXT        PIC 9(7).                                    
001900*                                 PRODUKTIONSNUMMER                       
002000     03 IDKOLLI-NEXT         PIC 9(5).                                    
002100*                                 KOLLINUMMER                             
002200     03 IDDISTR              PIC Z(3)9.                                   
002300*                                 DISTRIKTNUMMER                          
002400     03 IDKUNDNR             PIC Z(5)9.                                   
002500*                                 KUNDNUMMER                              
002600     03 IDDC                 PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800     03 IDSKEPPN             PIC Z(6)9.                                   
002900*                                 SKEPPNINGSNUMMER                        
003000     03 TILASTN-REL          PIC 9(6).                                    
003100*                                 RELEASEDATUM                            
003200     03 KDLBTYP              PIC Z(2)9.                                   
003300*                                 LASTBÄRARTYP                            
003400     03 IDLBBET              PIC X(12).                                   
003500*                                 LASTBÄRARBETECKNING                     
003600     03 IDLBSIGL             PIC X(9).                                    
003700*                                 LASTBÄRARE SIGILL                       
003800     03 VKTARA               PIC X(9).                                    
003900*                                 TARAVIKT (KG)                           
004000     03 VKTARA-NUM REDEFINES VKTARA                                       
004100                             PIC Z(6)9.9.                                 
004200*                                 TARAVIKT (KG)                           
004300     03 KOLLI-GRUPP.                                                      
004400        05 KOLLIN            OCCURS 20 TIMES                              
004500                             INDEXED IX.                                  
004600           07 IDPRODNR       PIC Z(6)9.                                   
004700*                                 PRODUKTIONSNUMMER                       
004800           07 IDKOLLI        PIC Z(4)9.                                   
004900*                                 KOLLINUMMER                             
005000           07 TILASTN-ATER   PIC Z(6).                                    
005100*                                 ÅTERRAPPORTERINGSDATUM                  
005200           07 BETEXT-AVVIK   PIC X(17).                                   
005300*                                                BETEXT-AVVIK-008         
005400*                                 AVIKELSEKOMMENTAR                       
005500     03 MESSAGE-RAD23        PIC X(79).                                   
005600*                                 MEDDELANDEFÄLT PÅ RAD 23                
005700*** END OF VILMAII-COPY LENGTH= 910 BYTES                                 
