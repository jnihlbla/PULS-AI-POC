000100* GENERATION OF COBOL HOST STRUCTURE FROM TP8LRET-TAB                     
000200  01 TP8LRET.                                                             
000300*              TP8LRET                                                    
000400   03 IDPARTNR        PIC X(9).                                           
000500*              PARTNERNUMMER                                              
000600   03 KDANMORS        PIC X(2).                                           
000700*              ORSAK TILL LEVERANSANMÄRKNING                              
000800   03 DAREGDAT        PIC X(8).                                           
000900*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
001000   03 IDREF           PIC X(15).                                          
001100*              REFERENS ID                                                
001200   03 IDEXCUST-1      PIC X(15).                                          
001300*              EXTERNT KUNDID                                             
001400   03 IDEXCUST-2      PIC X(15).                                          
001500*              EXTERNT KUNDID                                             
001600   03 IDLANDX3-SEND   PIC X(3).                                           
001700*              LANDKOD SÄNDANDE LAND                                      
001800   03 DAREFDAT        PIC X(8).                                           
001900*              REFERENSDATUM (ÅÅÅÅMMDD)                                   
002000   03 BEART           PIC X(25).                                          
002100*              ARTIKELBENÄMNING                                           
002200   03 PRARTNTO        PIC S9(7)V9(2) COMP-3.                              
002300*              ARTIKELPRIS NETTO                                          
002400   03 PRARTBTO        PIC S9(7)V9(2) COMP-3.                              
002500*              FÖRSÄLJNINGSPRIS BRUTTO (KR)                               
002600   03 KVLEVART        PIC S9(7) COMP-3.                                   
002700*              LEVERERAT ANTAL STYCK                                      
002800   03 KVBEART         PIC S9(7) COMP-3.                                   
002900*              BESTÄLLT ANTAL STYCKEN                                     
003000   03 KDVALISO        PIC X(3).                                           
003100*              VALUTAKOD ENLIGT ISO-STANDARD.                             
003200   03 IDOPTION-1      PIC X(15).                                          
003300*              BRYTBEGREPP                                                
003400   03 IDOPTION-2      PIC X(15).                                          
003500*              BRYTBEGREPP                                                
003600   03 IDOPTION-3      PIC X(15).                                          
003700*              BRYTBEGREPP                                                
003800   03 IDDC            PIC X(2).                                           
003900*              IDENTIFIERARE LAGER                                        
004000   03 IDUSER-1        PIC X(8).                                           
004100*              ANVÄNDARENS SÄKERHETS ID                                   
004200   03 IDUSER-2        PIC X(8).                                           
004300*              ANVÄNDARENS SÄKERHETS ID                                   
004400   03 IDSYSTEM-SEND   PIC X(4).                                           
004500*              VOLVO SÄNDANDE SYSTEM                                      
004600   03 IDSYSTEM-REC    PIC X(4).                                           
004700*              VOLVO MOTTAGANDE SYSTEM                                    
004800   03 DAUPPDAT        PIC X(8).                                           
004900*              UPPDATERINGSDATUM  (ÅÅÅÅMMDD)                              
005000   03 DADELDAT        PIC X(8).                                           
005100*              BORTTAGSDATUM      (ÅÅÅÅMMDD)                              
005200   03 DAFAKT          PIC X(8).                                           
005300*              FAKTURERINGSDATUM (ÅÅÅÅMMDD)                               
005400   03 IDFINDOC        PIC S9(9) COMP-3.                                   
005500*              FINANSIELLT DOKUMENT ID                                    
005600   03 KDRAPPSTA       PIC X(2).                                           
005700*              RAPPORTSTATUS HANDLING FEE                                 
005800   03 IDFTG           PIC X(2).                                           
005900*              FÖRETAGSID EKONOM REDOVISNING                              
006000*                                                                         
006100*** END OF VILMAII-COPY LENGTH= 225 OLD LENGTH=                           
