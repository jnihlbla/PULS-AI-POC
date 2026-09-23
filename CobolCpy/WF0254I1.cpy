000100 01  REQU-WF0254I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0254             
000300*                                 LEGAL SELLER MAINTENANCE                
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-KDSTATUS-KEY    PIC 9(3).                                    
000800*                                 STATUSKOD          KDSTATUS-002         
000900     03 REQU-FLCOMING        PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100*                                 GENERAL FLAG                            
001200     03 REQU-BELEG-NAME1     PIC X(35).                                   
001300*                                 DEL AV LEGAL SELLER NAMN                
001400*                                 PART OF LEGAL SELLER NAME               
001500     03 REQU-BELEG-NAME2     PIC X(35).                                   
001600*                                 DEL AV LEGAL SELLER NAMN                
001700*                                 PART OF LEGAL SELLER NAME               
001800     03 REQU-ADLEG-STREET    PIC X(35).                                   
001900*                                 LEGAL SELLER GATUADRESS                 
002000*                                 LEGAL SELLER STREET ADDRESS             
002100     03 REQU-ADLEG-BOX       PIC X(10).                                   
002200*                                 BOXADRESS LEGAL SELLER                  
002300*                                 LEGAL SELLER BOX ADDRESS                
002400     03 REQU-ADLEG-PCODE     PIC X(10).                                   
002500*                                 LEGAL SELLER ADRESS POSTNR              
002600*                                 LEGAL SELLER POSTAL CODE                
002700     03 REQU-ADLEG-CITY      PIC X(35).                                   
002800*                                 LEGAL SÄLJARES ADRESS STAD              
002900*                                 LEGAL SELLER ADDRESS CITY               
003000     03 REQU-IDLANDX3        PIC X(3).                                    
003100*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
003200*                                 3-LETTER CODE FOR COUNTRY.              
003300     03 REQU-IDTFN           PIC X(20).                                   
003400*                                 TELEFONNUMMER EXTERNT                   
003500*                                 TELEPHONE NUMBER  EXTERNAL              
003600     03 REQU-IDTFX           PIC X(20).                                   
003700*                                 TELEFAXNUMMER                           
003800*                                 FAXNUMBER                               
003900     03 REQU-IDMAIL          PIC X(60).                                   
004000*                                 MAIL ADRESS                             
004100*                                 MAIL ADDRESS                            
004200     03 REQU-BECONT          PIC X(35).                                   
004300*                                 KONTAKTPERSON                           
004400*                                 CONTACT PERSON                          
004500     03 REQU-IDVAT           PIC X(17).                                   
004600*                                 MOMSREGISTRERINGSNUMMER                 
004700*                                 VAT REGISTRATION NUMBER                 
004800     03 REQU-IDBG            PIC X(15).                                   
004900*                                 BANKGIRO                                
005000*                                 BANC CHEQUE ACCOUNT                     
005100     03 REQU-IDPG            PIC X(15).                                   
005200*                                 POSTGIRO                                
005300*                                 POSTAL CHEQUE ACCOUNT                   
005400     03 REQU-KDINVFRQ        PIC X(4).                                    
005500*                                 FAKTURERINGSFREKVENS                    
005600*                                 INVOICE FREQUENCE                       
005700     03 REQU-KDAPPEND        PIC X(4).                                    
005800*                                 APPENDIX KOD                            
005900*                                 APPENDIX CODE                           
006000     03 REQU-FLSLUT          PIC X.                                       
006100*                                 AVSLUTNINGSFLAGGA                       
006200     03 REQU-FLCUSUPD        PIC X.                                       
006300*                                 FINANSIELL KUNDUPPDATERING              
006400*                                                                         
006500*                                 FINANCIAL CUSTOMER UPDATING             
006600*                                                                         
006700     03 REQU-FLVATUPD        PIC X.                                       
006800*                                 VAT UPPDATERING                         
006900*                                 VAT UPDATING                            
007000     03 REQU-DAUPPDAT        PIC X(8).                                    
007100*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
007200*                                 UPDATING DATE     (YYYYMMDD)            
007300     03 REQU-FLCURRCL        PIC X.                                       
007400*                                 VALUTAN RENSAS                          
007500*                                 CURRENCY CLEAN UP FLAG                  
007600     03 REQU-FLINTODO        PIC X.                                       
007700*                                 FÖRSÄKRINGSTEXT PÅ DOKUMENT             
007800*                                 INSURANCE TEXT TO DOCUMENT              
007900     03 REQU-KDVALISO        PIC X(3).                                    
008000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008100*                                 CURRENCY CODE BY ISO-STANDARD.          
008200     03 REQU-KDTRADP         PIC X(4).                                    
008300*                                 TRADING PARTNER                         
008400*                                 TRADING PARTNER                         
008500*** END OF VILMAII-COPY LENGTH= 381 BYTES                                 
