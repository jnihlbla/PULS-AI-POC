//W488J022  JOB (670W4880100W488J022,W100),'RTN W488S1',                        
//             USER=?,PASSWORD=?,                                               
//          CLASS=1                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM TIME=1,LINES=5,CARDS=0,FORMS=1800                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W488    EXEC W488P022                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W488J022                                         
