//W115JPV5 JOB (540W1150100VILM2P),'RTN W115S1  ',                              
//             CLASS=K                                                          
/*JOBPARM ROOM=W200,TIME=1,LINES=99,CARDS=0,FORMS=1800                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//SORTALC EXEC SORT025M                                                         
//*                                                                             
//******* SPECIAL BESTÄLLNING BASLAGERLISTA FÖR AVD 57620                       
//******* ALT VÄRDE FÖR DEST1=RXXXXX,SYSOUT='(A,,STD)' =INTERNT                 
//******* ALT VÄRDE FÖR DEST1=NJOCPVC,SYSOUT='(A,,1001)' =VD                    
//*                                                                             
//******* FÖR FLER KOPIOR, ÄNDRA I EXPRESS DELIVERY "COPIES"                    
//*                                                                             
//W115    EXEC W115P005,                                                        
//             DEST1=NJOCP,SYSOUT='(A,,1001)'                                   
//KORTIN DD *                                                                   
00143ARE 11E19200000000000000GB    IDFKNGRP  IDARTNR   TISTOMREG                
