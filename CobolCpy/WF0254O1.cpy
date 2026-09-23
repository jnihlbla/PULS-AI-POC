000100 01  RESP-WF0254O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0254         
000300*                                 LEGAL SELLER MAINTENANCE                
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-KDSTATUS-KEY    PIC 9(3).                                    
000800*                                 STATUSKOD          KDSTATUS-002         
000900     03 RESP-FLCOMING        PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100*                                 GENERAL FLAG                            
001200     03 RESP-BELEG-NAME1     PIC X(35).                                   
001300*                                 DEL AV LEGAL SELLER NAMN                
001400*                                 PART OF LEGAL SELLER NAME               
001500     03 RESP-BELEG-NAME2     PIC X(35).                                   
001600*                                 DEL AV LEGAL SELLER NAMN                
001700*                                 PART OF LEGAL SELLER NAME               
001800     03 RESP-ADLEG-STREET    PIC X(35).                                   
001900*                                 LEGAL SELLER GATUADRESS                 
002000*                                 LEGAL SELLER STREET ADDRESS             
002100     03 RESP-ADLEG-BOX       PIC X(10).                                   
002200*                                 BOXADRESS LEGAL SELLER                  
002300*                                 LEGAL SELLER BOX ADDRESS                
002400     03 RESP-ADLEG-PCODE     PIC X(10).                                   
002500*                                 LEGAL SELLER ADRESS POSTNR              
002600*                                 LEGAL SELLER POSTAL CODE                
002700     03 RESP-ADLEG-CITY      PIC X(35).                                   
002800*                                 LEGAL SÄLJARES ADRESS STAD              
002900*                                 LEGAL SELLER ADDRESS CITY               
003000     03 RESP-IDLANDX3        PIC X(3).                                    
003100*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
003200*                                 3-LETTER CODE FOR COUNTRY.              
003300     03 RESP-BELAND          PIC X(35).                                   
003400*                                 LANDSBETECKNING                         
003500*                                 NAME OF COUNTRY                         
003600     03 RESP-IDTFN           PIC X(20).                                   
003700*                                 TELEFONNUMMER EXTERNT                   
003800*                                 TELEPHONE NUMBER  EXTERNAL              
003900     03 RESP-IDTFX           PIC X(20).                                   
004000*                                 TELEFAXNUMMER                           
004100*                                 FAXNUMBER                               
004200     03 RESP-IDMAIL          PIC X(60).                                   
004300*                                 MAIL ADRESS                             
004400*                                 MAIL ADDRESS                            
004500     03 RESP-BECONT          PIC X(35).                                   
004600*                                 KONTAKTPERSON                           
004700*                                 CONTACT PERSON                          
004800     03 RESP-IDVAT           PIC X(17).                                   
004900*                                 MOMSREGISTRERINGSNUMMER                 
005000*                                 VAT REGISTRATION NUMBER                 
005100     03 RESP-IDBG            PIC X(15).                                   
005200*                                 BANKGIRO                                
005300*                                 BANC CHEQUE ACCOUNT                     
005400     03 RESP-IDPG            PIC X(15).                                   
005500*                                 POSTGIRO                                
005600*                                 POSTAL CHEQUE ACCOUNT                   
005700     03 RESP-KDINVFRQ        PIC X(4).                                    
005800*                                 FAKTURERINGSFREKVENS                    
005900*                                 INVOICE FREQUENCE                       
006000     03 RESP-KDAPPEND        PIC X(4).                                    
006100*                                 APPENDIX KOD                            
006200*                                 APPENDIX CODE                           
006300     03 RESP-FLSLUT          PIC X.                                       
006400*                                 AVSLUTNINGSFLAGGA                       
006500     03 RESP-FLCUSUPD        PIC X.                                       
006600*                                 FINANSIELL KUNDUPPDATERING              
006700*                                                                         
006800*                                 FINANCIAL CUSTOMER UPDATING             
006900*                                                                         
007000     03 RESP-FLVATUPD        PIC X.                                       
007100*                                 VAT UPPDATERING                         
007200*                                 VAT UPDATING                            
007300     03 RESP-DAREGDAT        PIC Z(8).                                    
007400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
007500*                                 REGISTRATION DATE (YYYYMMDD)            
007600     03 RESP-DAUPPDAT        PIC Z(8).                                    
007700*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
007800*                                 UPDATING DATE     (YYYYMMDD)            
007900     03 RESP-IDUSER          PIC X(8).                                    
008000*                                 ANVÄNDARENS SÄKERHETS ID                
008100*                                 USER SECURITY-IDENTITY                  
008200     03 RESP-FLCURRCL        PIC X.                                       
008300*                                 VALUTAN RENSAS                          
008400*                                 CURRENCY CLEAN UP FLAG                  
008500     03 RESP-FLINTODO        PIC X.                                       
008600*                                 FÖRSÄKRINGSTEXT PÅ DOKUMENT             
008700*                                 INSURANCE TEXT TO DOCUMENT              
008800     03 RESP-KDVALISO        PIC X(3).                                    
008900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009000*                                 CURRENCY CODE BY ISO-STANDARD.          
009100     03 RESP-KDTRADP         PIC X(4).                                    
009200*                                 TRADING PARTNER                         
009300*                                 TRADING PARTNER                         
009400*** END OF VILMAII-COPY LENGTH= 432 BYTES                                 
