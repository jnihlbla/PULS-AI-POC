//W221B016 JOB (640W2210100W221J016,W100),'RTN PER-ANDERS HB2N',                
//             CLASS=K,MSGCLASS=H,NOTIFY=R040073                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//***************************************************************               
//* STARTAR BUNDELJOBB FÖR PRINTNING.                           *               
//***************************************************************               
//RMOJCL  EXEC PGM=RMOBPR,PARM='&INDEXD,W221B016'                               
//*                                                                             
//*OPEND  EXEC WSOPEND,PROCESS=W221J016                                         
