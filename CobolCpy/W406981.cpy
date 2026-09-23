000100 01  W406981.                                                             
000200*                                 COPYTEXT FOR BOLLA LDC HEAD LIN         
000300*                                 E                                       
000400     03 HEAD-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 HEAD-BEBETRAD        PIC X(35).                                   
000700*                                 DEL AV BETALNINGSANSVARIGS NAMN         
000800     03 HEAD-BEGMT-RAD1      PIC X(35).                                   
000900*                                 GODSMOTTAGARNAMN RAD 1                  
001000     03 HEAD-ADBET-STREET    PIC X(35).                                   
001100*                                 BETALARENS GATUADRESS                   
001200     03 HEAD-ADGMT-GATA      PIC X(35).                                   
001300*                                 GODSMOTTAGARADRESS GATA                 
001400     03 HEAD-ADBET-CITY      PIC X(35).                                   
001500*                                 BETALARENS STADSADRESS                  
001600     03 HEAD-ADGMT-PADR      PIC X(35).                                   
001700*                                 GODSMOTTAGARADRESS POSTADRESS           
001800     03 HEAD-KVKOLLI         PIC Z(4).                                    
001900*                                 ANTAL KOLLI                             
002000     03 HEAD-VKORDBTO        PIC Z(6).Z.                                  
002100*                                 ORDERVIKT BRUTTO (KG)                   
002200     03 HEAD-BEEMBTYP-5      PIC X(12).                                   
002300*                                 EMBALLAGETYPSTEXT  BEEMBTYP-002         
002400     03 HEAD-BEEMBTYP-6      PIC X(12).                                   
002500*                                 EMBALLAGETYPSTEXT  BEEMBTYP-002         
002600     03 HEAD-BEEMBTYP-7      PIC X(12).                                   
002700*                                 EMBALLAGETYPSTEXT  BEEMBTYP-002         
002800     03 HEAD-BEEMBTYP-8      PIC X(12).                                   
002900*                                 EMBALLAGETYPSTEXT  BEEMBTYP-002         
003000     03 HEAD-BEEMBTYP-9      PIC X(12).                                   
003100*                                 EMBALLAGETYPSTEXT  BEEMBTYP-002         
003200     03 HEAD-BEEMBTYP-10     PIC X(12).                                   
003300*                                 EMBALLAGETYPSTEXT  BEEMBTYP-002         
003400     03 HEAD-KVEMBTYP-5      PIC Z(3).                                    
003500*                                 FÖRBRUKNINGMÄNGD AV EMBALLAGE           
003600     03 HEAD-KVEMBTYP-6      PIC Z(3).                                    
003700*                                 FÖRBRUKNINGMÄNGD AV EMBALLAGE           
003800     03 HEAD-KVEMBTYP-7      PIC Z(3).                                    
003900*                                 FÖRBRUKNINGMÄNGD AV EMBALLAGE           
004000     03 HEAD-KVEMBTYP-8      PIC Z(3).                                    
004100*                                 FÖRBRUKNINGMÄNGD AV EMBALLAGE           
004200     03 HEAD-KVEMBTYP-9      PIC Z(3).                                    
004300*                                 FÖRBRUKNINGMÄNGD AV EMBALLAGE           
004400     03 HEAD-KVEMBTYP-10     PIC Z(3).                                    
004500*                                 FÖRBRUKNINGMÄNGD AV EMBALLAGE           
004600     03 HEAD-PORTO-1         PIC X(17).                                   
004700     03 HEAD-PORTO-2         PIC X(17).                                   
004800     03 HEAD-PORTO-3         PIC X(17).                                   
004900     03 HEAD-ASPETTO         PIC X(17).                                   
005000     03 HEAD-BETRPFIR        PIC X(45).                                   
005100*                                 TRANSPORTFIRMANS NAMN                   
005200     03 HEAD-ADTRPFIR-RAD1   PIC X(35).                                   
005300*                                 TRANSPORTFIRMA ADRESSRAD-1              
005400     03 HEAD-ADTRPFIR-RAD2   PIC X(35).                                   
005500*                                 TRANSPORTFIRMA ADRESSRAD-2              
005600     03 HEAD-CAUSALE         PIC X(35).                                   
005700     03 HEAD-DATRP           PIC 9(8).                                    
005800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
005900     03 HEAD-TITRP           PIC Z9.9(2).                                 
006000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
006100     03 HEAD-IDKUNDNR        PIC Z(5)9.                                   
006200*                                 KUNDNUMMER                              
006300     03 HEAD-IDTRPBOT        PIC X.                                       
006400*                                 BOLLA-DOKUMENT TECKEN                   
006500     03 HEAD-IDTRPBON        PIC X(7).                                    
006600*                                 BOLLA-DOKUMENT NUMMER                   
006700     03 HEAD-DABOLLA         PIC 9(8).                                    
006800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
006900     03 HEAD-COD-NOTE        PIC X(35).                                   
007000     03 HEAD-KDORDKL-TEXT    PIC X(14).                                   
007100     03 HEAD-INSTR-NOTE-13   PIC X(35).                                   
007200     03 HEAD-INSTR-NOTE-14   PIC X(35).                                   
007300     03 HEAD-IDORDNR         PIC Z(4)9.                                   
007400*                                 ORDERNUMMER UTGÅR PD90                  
007500*** END OF VILMAII-COPY LENGTH= 699 BYTES                                 
