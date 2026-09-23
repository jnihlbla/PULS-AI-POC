//W980J046   JOB (540W0090100W980J046,W100),'RTN W980R2',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800                                                            
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//ACF2SET EXEC FLOGON                                                           
//SYSTSPRT DD  SYSOUT=M                                                         
//SYSTSIN  DD *                                                                 
 ACF                                                                            
 SET LID                                                                        
 CHANGE &USER PASSWORD(&NEWPWD)                                                 
 END                                                                            
//*                                                                             
//SOPSET  EXEC WSOP                                                             
 SET VALUE *SOP*                                                                
   PASSWORD(&NEWNBR)                                                            
 END-SET                                                                        
//*                                                                             
//SOPSET  EXEC WSOP,                 -- PMC SOP ALSO                            
//             SOPREG=W.PMC.SOP                                                 
 SET VALUE *SOP*                                                                
   PASSWORD(&NEWNBR)                                                            
 END-SET                                                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W980J046                                         
//*                                                                             
//*                                                                             
//W980JVC1   JOB (540W0090100W980JVC1,W100),'RTN W980R2',                       
//             USER=W0VCOM1,PASSWORD=(&OLDPWD,&NEWPWD),                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800                                                            
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC PGM=IEFBR14                                                      
//*                                                                             
//*                                                                             
//W980JVC2   JOB (540W0090100W980JVC2,W100),'RTN W980R2',                       
//             USER=W0VCOM2,PASSWORD=(&OLDPWD,&NEWPWD),                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800                                                            
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC PGM=IEFBR14                                                      
