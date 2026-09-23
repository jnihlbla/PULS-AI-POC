000100 01  W412002.                                                             
000200*                                 ID-DEL FÖR PT 002                       
000300*                                 POSTER FRÅN WDG6 SOM HAR                
000400*                                 KDPRTYP = P                             
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDSYSTEM             PIC X(4).                                    
000800*                                 SKAPANDE SYSTEMNUMMER                   
000900     03 IDUSER               PIC X(8).                                    
001000*                                 ANVÄNDARENS SÄKERHETS ID                
001100     03 IDORDER              PIC S9(7)           COMP-3.                  
001200*                                 VOLVO PARTS ORDERNUMMER                 
001300     03 IDDISTR              PIC S9(5)           COMP-3.                  
001400*                                 DISTRIKTNUMMER                          
001500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700     03 IDKUNDRF             PIC X(10).                                   
001800*                                 KUNDENS REFERENS (ORDERID)              
001900     03 IDDC                 PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 IDLOPNR              PIC S9(3)           COMP-3.                  
002400*                                 LÖPNUMMER                               
002500     03 KDORDKL              PIC S9              COMP-3.                  
002600*                                 ORDERKLASS                              
002700     03 KVBEART              PIC S9(7)           COMP-3.                  
002800*                                 BESTÄLLT ANTAL STYCKEN                  
002900     03 KVLEVART             PIC S9(7)           COMP-3.                  
003000*                                 LEVERERAT ANTAL STYCK                   
003100     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
003200*                                 ARTIKELPRIS NETTO                       
003300     03 KDPRTYP              PIC X.                                       
003400*                                 TYP AV PRISTILLÄMPNING                  
003500     03 FLFORBI              PIC X.                                       
003600*                                 FÖRBIORDERFLAGGA                        
003700     03 BEART                PIC X(25).                                   
003800*                                 ARTIKELBENÄMNING                        
